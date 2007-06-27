/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.project;

import java.util.*;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class Watcher implements XModelTreeListener {
	static String[] CONTRIBUTORS = new String[]{
		"org.jboss.tools.struts.webprj.model.helpers.sync.SyncProjectContext",
		"org.jboss.tools.jsf.web.JSFWatcherContributor",
	};

    public static Watcher getInstance(XModel model) {
		Watcher instance = (Watcher)model.getManager("Watcher");
        if(instance == null) {
			instance = new Watcher();
            instance.setModel(model);
			model.addManager("Watcher", instance);
            model.addModelTreeListener(instance);
        }
        return instance;
    }

    protected XModel model;
    protected IWatcherContributor[] contributors = new IWatcherContributor[0];
    private boolean lock = false;
/*	
	Job job;
	
	class WatcherJob extends Job {
		public WatcherJob() {
			super("Watcher" + XModelConstants.getWorkspace(model));
		}
		protected IStatus run(IProgressMonitor monitor) {
			Watcher.this.updateAll();
			return Status.OK_STATUS;
		}
		
	};
*/
	class WatcherRunnable implements XJob.XRunnable {
		String id = "Watcher - " + XModelConstants.getWorkspace(model);

		public String getId() {
			return id;
		}

		public void run() {
			Watcher.this.updateAll();
		}
		
	}
    private Watcher() {
		loadContributors();
    }
    
    void loadContributors() {
    	ArrayList<IWatcherContributor> list = new ArrayList<IWatcherContributor>();
    	for (int i = 0; i < CONTRIBUTORS.length; i++) {
    		try {
    			Object watcher = ModelFeatureFactory.getInstance().createFeatureInstance(CONTRIBUTORS[i]);
    			if(watcher instanceof IWatcherContributor)
    				list.add((IWatcherContributor)watcher);
    			else
					if(ModelPlugin.isDebugEnabled()) {			
						ModelPlugin.getPluginLog().logInfo("Class is not implemented IWatcherContributor interface!");
					}
    		} catch (Exception e) {
    			ModelPlugin.getPluginLog().logError(e);
    		}
    	}
    	contributors = list.toArray(new IWatcherContributor[0]);
    }

    public void setModel(XModel model) {
        this.model = model;
        for (int i = 0; i < contributors.length; i++) {
        	contributors[i].init(model);
        }
//		job = new WatcherJob();
    }
    
    public void forceUpdate() {
    	if(model.getProperties().getProperty(IModelNature.ECLIPSE_PROJECT) == null) return;
    	XJob.addRunnable(new WatcherRunnable());
//		if(job.getState() == Job.NONE) {
//			job.schedule(600);
//		};
    }

    private void updateAll() {
        if(lock) return;
        lock();
        try {
        	String err = null;
        	for (int i = 0; i < contributors.length; i++) {
        		if(!contributors[i].isActive()) continue;
				contributors[i].update();
				if(err == null) {
					err = contributors[i].getError();
				}
        	}
        	setError(err);
            setCorrect(err == null);
			for (int i = 0; i < contributors.length; i++) {
				if(!contributors[i].isActive()) continue;
				contributors[i].updateProject();
			}
        } finally {
            unlock();
        }
    }

    public void lock() {
        lock = true;
    }

    public void unlock() {
        lock = false;
    }

    public void structureChanged(XModelTreeEvent event) {
        XModel model = event.getModelObject().getModel();
        if (event.kind() == XModelTreeEvent.STRUCTURE_CHANGED &&
                event.getModelObject() == model.getRoot()) {
            model.removeModelTreeListener(this);
///            stopped = true;
            model.removeManager("Watcher");
            return;
        }
        forceUpdate();
    }
    
    public void nodeChanged(XModelTreeEvent event) {
    	forceUpdate();
    }

    
	String error = "initial";
	ResourceMarkers markers = null;

	class RM extends ResourceMarkers {
		public RM() {
			super(ResourceMarkers.JST_WEB_PROBLEM);
		}
		protected String[] getErrors() {
			return (error == null) ? new String[0] : new String[]{error};
		}
	}

	public void setError(String err) {
		if(markers == null) {
			markers = new RM();
			markers.setModelObject(FileSystemsHelper.getFileSystems(model));
			markers.clear();
		}
		boolean changed = (error == null) ? (err != null) : !error.equals(err);
		if(changed) {
			error = err;
			markers.update();
		}
	}

	private void setCorrect(boolean correct) {
		XModelObject fs = FileSystemsHelper.getFileSystems(model);
		if(fs == null) return;
		boolean b = !"yes".equals(fs.get("_hasErrors_"));
		if(b == correct) return;
		fs.set("_hasErrors_", (correct) ? "" : "yes");
		fs.fireObjectChanged("_hasErrors_");
	}

}
