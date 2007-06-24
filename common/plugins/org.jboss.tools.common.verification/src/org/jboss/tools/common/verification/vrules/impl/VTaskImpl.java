/*
 * VTaskImpl.java
 *
 * Created on July 24, 2003, 5:08 PM
 */

package org.jboss.tools.common.verification.vrules.impl;

import java.util.ArrayList;
import java.util.List;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.verification.vrules.*;
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
            thread = new Thread(this, "VTask");
            thread.start();
        } else {
            if(sleeping) {
                synchronized(this) {
                    sleeping = false;
                    try { notifyAll(); } catch (Exception e) {}
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
        for (index = 0; index < rules.length; index++) {
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
                    try { wait(); } catch (Exception e) {}
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
                if (action == null) continue;// should not happen
                try {
                    VResult[] results = action.check(object);
                    notifyApplied(rule, object, results);
                } catch (Exception e) {
                	if(VerificationPlugin.isDebugEnabled()) {
						ModelPlugin.log("Exception in action "+action+" ignored: "+e, e);
                	}
                }
                checked = true;
                if (checkChildren) break;
            } else if (entity.isDescendant(entities[i].getName())) {
                checkChildren = true;
                if (checked) break;
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
                VTaskListener listener = (VTaskListener)listeners.get(i);
                try {
                    listener.onStart();
                } catch (Exception e) {
					logListener(listener, e);
                }
            }
        }
    }
    
    private void logListener(VTaskListener listener, Throwable e) { 
		if(VerificationPlugin.isDebugEnabled()) {
			ModelPlugin.log("Exception in listener "+listener+" ignored: "+e, e);
		}
    }

    private void notifyApplied(VRule rule, VObject object, VResult[] results) {
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                VTaskListener listener = (VTaskListener)listeners.get(i);
                try {
                    listener.onRuleApplied(rule, object, results);
                } catch (Exception e) {
					logListener(listener, e);
                }
            }
        }
    }

    private void notifyRuleFinished(VRule rule, VObject object) {
		if(!running) return;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                VTaskListener listener = (VTaskListener)listeners.get(i);
                try {
                    listener.onRuleFinished(rule, object);
                } catch (Exception e) {
					logListener(listener, e);
                }
            }
        }
    }

    private void notifyPause() {
    	if(!running) return;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                VTaskListener listener = (VTaskListener)listeners.get(i);
                try {
                    listener.onPause();
                } catch (Exception e) {
					logListener(listener, e);
                }
            }
        }
    }

    private void notifyResume() {
		if(!running) return;
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                VTaskListener listener = (VTaskListener)listeners.get(i);
                try {
                    listener.onResume();
                } catch (Exception e) {
					logListener(listener, e);
                }
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
            try {
                ls[i].onFinish();
            } catch (Exception e) {
				logListener(ls[i], e);
            }
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
    
}
