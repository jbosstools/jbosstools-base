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
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class Watcher implements XModelTreeListener {
    public static Watcher getInstance(XModel model) {
		Watcher instance = (Watcher)model.getManager("Watcher"); //$NON-NLS-1$
        if(instance == null) {
			instance = new Watcher();
            instance.setModel(model);
			model.addManager("Watcher", instance); //$NON-NLS-1$
            model.addModelTreeListener(instance);
        }
        return instance;
    }

    protected XModel model;
    protected Map<String,IWatcherContributor> contributors = new HashMap<String, IWatcherContributor>();
    private boolean lock = false;

	class WatcherRunnable implements XJob.XRunnable {
		String id = "Watcher - " + XModelConstants.getWorkspace(model); //$NON-NLS-1$

		public String getId() {
			return id;
		}

		public void run() {
			Watcher.this.updateAll();
		}
		
	}
	
    private Watcher() {
    }
    
    void updateContributors() {
    	if(model == null) return;
    	ModelNatureExtension[] es = ModelNatureExtension.getInstances();
    	for (int i = 0; i < es.length; i++) {
    		String nature = es[i].getName();
    		if(EclipseResourceUtil.hasNature(model, nature)) {
    			if(contributors.containsKey(nature)) {
    				continue;
    			} else {
    				String contributorName = es[i].getWatcherContributor();
   	    			Object watcher = ModelFeatureFactory.getInstance().createFeatureInstance(contributorName);
   	    			if(watcher instanceof IWatcherContributor) {
   	    				IWatcherContributor c = (IWatcherContributor)watcher;
   	    				c.init(model);
   	    				contributors.put(nature, c);
   	    			} else if(ModelPlugin.isDebugEnabled()) {			
   						ModelPlugin.getPluginLog().logInfo("Class is not implemented IWatcherContributor interface!"); //$NON-NLS-1$
  					}
    			}
    		} else {
    			contributors.remove(nature);
    		}
    	}
    }

    public void setModel(XModel model) {
        this.model = model;
    }
    
    public void forceUpdate() {
    	if(model.getProperties().getProperty(IModelNature.ECLIPSE_PROJECT) == null) return;
    	XJob.addRunnable(new WatcherRunnable());
    }

    private void updateAll() {
        if(lock) return;
        lock();
        updateContributors();
        try {
        	String err = null;
        	for (IWatcherContributor c : contributors.values()) {
        		if(!c.isActive()) continue;
        		c.update();
				if(err == null) {
					err = c.getError();
				}
        	}
        	setError(err);
            setCorrect(err == null);
        	for (IWatcherContributor c : contributors.values()) {
        		if(!c.isActive()) continue;
				c.updateProject();
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
            model.removeManager("Watcher"); //$NON-NLS-1$
            return;
        }
        forceUpdate();
    }
    
    public void nodeChanged(XModelTreeEvent event) {
    	forceUpdate();
    }

    
	String error = "initial"; //$NON-NLS-1$
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
		boolean b = !XModelObjectConstants.YES.equals(fs.get("_hasErrors_")); //$NON-NLS-1$
		if(b == correct) return;
		fs.set("_hasErrors_", (correct) ? "" : XModelObjectConstants.YES); //$NON-NLS-1$ //$NON-NLS-2$
		fs.fireObjectChanged("_hasErrors_"); //$NON-NLS-1$
	}

}
