/*
 * VTaskImpl.java
 *
 * Created on July 24, 2003, 5:08 PM
 */

package org.jboss.tools.common.verification.vrules.impl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.jboss.tools.common.verification.vrules.VAction;
import org.jboss.tools.common.verification.vrules.VEntity;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VManager;
import org.jboss.tools.common.verification.vrules.VObject;
import org.jboss.tools.common.verification.vrules.VResult;
import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.vrules.VTask;
import org.jboss.tools.common.verification.vrules.VTaskListener;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

/**
 *
 * @author  valera
 */
public class VTaskImpl implements VTask, Runnable {
    
    private VObject object;
    private VRule[] rules;
    private int index = 0;
    private List<VTaskListener> listeners = new ArrayList<VTaskListener>();
    private Thread thread;
    private boolean running = false;
    private boolean sleeping = false;

    /** Creates a new instance of VTaskImpl */
    public VTaskImpl(VManager manager, VObject object) {
        this.object = object;
        this.rules = VHelper.getRules(manager, object);
    }

    public VTaskImpl(VObject object, VRule[] rules) {
        this.object = object;
        this.rules = rules;
    }

    public void start() {
        if (thread == null) {
            thread = new Thread(this, "VTask"); //$NON-NLS-1$
            thread.start();
        } else {
            if(sleeping) {
                synchronized(this) {
                    sleeping = false;
                    try {
                    	notifyAll();
                    } catch (IllegalMonitorStateException e) {
                    	//ignore
                    }
                }
            }
        }
    }

    public boolean isSleeping() {
        return sleeping;
    }

    public boolean isRunning() {
        return running;
    }

    public synchronized void pause() {
        if(running && !sleeping) sleeping = true;
    }

    public void stop() {
        sleeping = false;
        running = false;
    }

    public void run() {
        notifyStarted();
        if(rules != null) for (index = 0; index < rules.length; index++) {
            if (!running) break;
            VRule rule = rules[index];
            if (rule.isEnabled() && rule.getAction() != null
                && rule.getRuleSet().isEnabled()) {
                apply(rule, object);
            }
            notifyRuleFinished(rule, object);
        }
        notifyFinish();
    }

    private void checkSleeping() {
        if(sleeping) {
            synchronized(this) {
                if(sleeping) {
                    notifyPause();
                    try {
                    	wait();
                    } catch (InterruptedException e) {
                    	//ignore
                    }
                    if(!sleeping) notifyResume();
                }
            }
        }
    }

    private void apply(VRule rule, VObject object) {
        VEntity entity = object.getEntity();
        VEntity[] entities = rule.getEntities();
        boolean checkChildren = false;
        boolean checked = false;
        for (int i = 0; i < entities.length; i++) {
            if(!running) return;
            checkSleeping();
            if(!running) return;
            if (entity != null && entities[i] != null && entity.getName().equals(entities[i].getName())) {
                VAction action = rule.getAction();
                if (action == null) {
                	continue;// should not happen
                }
                VResult[] results = action.check(object);
                notifyApplied(rule, object, results);
                checked = true;
                if (checkChildren) {
                	break;
                }
            } else if (entity.isDescendant(entities[i].getName())) {
                checkChildren = true;
                if (checked) {
                	break;
                }
            }
        }
        if (checkChildren) {
            VObject[] children = object.getChildren();
            for (int i = 0; i < children.length; i++) {
                apply(rule, children[i]);
            }
        }
    }

    private void notifyStarted() {
        running = true;
        sleeping = false;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                SafeRunner.run(new OnStartNotifier((VTaskListener)listeners.get(i)));
            }
        }
    }
    
    private void logListener(VTaskListener listener, Throwable e) { 
		VerificationPlugin.getPluginLog().logInfo("Exception in listener "+listener+" ignored: "+e, e); //$NON-NLS-1$ //$NON-NLS-2$
    }

    private void notifyApplied(VRule rule, VObject object, VResult[] results) {
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
               SafeRunner.run(new OnRuleAppliedNotifier((VTaskListener)listeners.get(i),rule, object, results));
            }
        }
    }

    private void notifyRuleFinished(VRule rule, VObject object) {
		if(!running) return;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
               SafeRunner.run(new OnRuleFinishedNotifier((VTaskListener)listeners.get(i),rule, object));
            }
        }
    }

    private void notifyPause() {
    	if(!running) return;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                SafeRunner.run(new OnPauseNotifier((VTaskListener)listeners.get(i)));
            }
        }
    }

    private void notifyResume() {
		if(!running) return;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                SafeRunner.run(new OnResumeNotifier((VTaskListener)listeners.get(i)));
            }
        }
    }

    private void notifyFinish() {
        sleeping = false;
        running = false;
        thread = null;
		VTaskListener[] ls = null; 
        synchronized (listeners) {
        	ls = (VTaskListener[])listeners.toArray(new VTaskListener[0]);
        }
        for (int i = 0; i < ls.length; i++) {
            SafeRunner.run(new OnFinishNotifier(ls[i]));
        }
    }

    public void addTaskListener(VTaskListener listener) {
        synchronized (listeners) {
            listeners.add(listener);
        }
    }
    
    public void removeTaskListener(VTaskListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }
    
    public class SafeNotifier implements ISafeRunnable {
    	
		protected VTaskListener listener;
		
		public SafeNotifier(VTaskListener listener) {
			this.listener = listener;
		}
		public void run() throws Exception {
		       listener.onStart();
		}
		public void handleException(Throwable exception) {
			logListener(listener, exception);
		}
    }
    
    public class OnStartNotifier extends SafeNotifier {
		public OnStartNotifier(VTaskListener listener) {
			super(listener);
		}
		public void run() throws Exception {
		       listener.onStart();
		}
    }
    
    public class OnPauseNotifier extends SafeNotifier {
		public OnPauseNotifier(VTaskListener listener) {
			super(listener);
		}
		public void run() throws Exception {
		       listener.onPause();
		}
    }
    
    public class OnResumeNotifier extends SafeNotifier {
		public OnResumeNotifier(VTaskListener listener) {
			super(listener);
		}
		public void run() throws Exception {
		       listener.onResume();
		}
    }
    
    public class OnFinishNotifier extends SafeNotifier {
		public OnFinishNotifier(VTaskListener listener) {
			super(listener);
		}
		public void run() throws Exception {
		       listener.onFinish();
		}
    }
    
    public class OnRuleFinishedNotifier extends SafeNotifier {
    	VRule rule;
    	VObject object; 
		public OnRuleFinishedNotifier(VTaskListener listener,VRule rule, VObject object) {
			super(listener);
			this.rule = rule;
			this.object = object;
		}
		public void run() throws Exception {
		       listener.onRuleFinished(rule, object);
		}
    }
    
    public class OnRuleAppliedNotifier extends OnRuleFinishedNotifier {
    	VResult[] results;
		public OnRuleAppliedNotifier(VTaskListener listener,VRule rule, VObject object, VResult[] results) {
			super(listener,rule,object);
			this.results =results;
		}
		public void run() throws Exception {
		       listener.onRuleApplied(rule, object, results);
		}
    }
    

}
