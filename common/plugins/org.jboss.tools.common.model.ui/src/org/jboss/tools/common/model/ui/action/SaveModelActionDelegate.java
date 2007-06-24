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
package org.jboss.tools.common.model.ui.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;

import org.jboss.tools.common.meta.action.impl.handlers.SaveAllHandler;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.RootImpl;

public class SaveModelActionDelegate extends AbstractModelActionDelegate {
	ModifyUpdate update = null;
	
	public SaveModelActionDelegate() {}

	protected void doRun() throws Exception {
		(new SaveAllHandler()).executeHandler(object, null);
	}

	protected void safeSelectionChanged(IAction action, ISelection selection) {
		if(object == null && action.isEnabled()) action.setEnabled(false);
		XModelObject adapter = getAdapter(selection);
		if(adapter == null) return;
		object = adapter;
		if(object != null && !object.isActive()) object = null;
		XModel model = (object != null) ? object.getModel() : null;
		update.setData(action, model);
		synchronized (monitor) {
			try { monitor.notifyAll(); } catch (Exception e) {}
		}
	}
	
	protected boolean computeEnabled() {
		return true;
	}
	
	public void dispose() {
		if(update != null) {
			update.stopped = true;
			synchronized (monitor) {
				try { monitor.notifyAll(); } catch (Exception e) {}
			}
			update = null;
		}
	}

	public void init(IWorkbenchWindow window) {
		update = new ModifyUpdate();
		update.start();
	}
	
	Object monitor = new Object();
	
	class ModifyUpdate extends Thread {
		IAction action;
		XModel model;
		boolean stopped = false;
		
		public ModifyUpdate() {
			super("Update Save Model Action");
		}
		
		public void setData(IAction action, XModel model) {
			this.action = action;
			if(this.model == model) return;
			if(this.model != null) {
				RootImpl r = (RootImpl)this.model.getRoot();
				r.removeModifyListener(monitor);
			}
			this.model = model;
			if(this.model != null) {
				RootImpl r = (RootImpl)this.model.getRoot();
				r.addModifyListener(monitor);
				XModelObject fs = model.getByPath("FileSystems");
				String fsn = (fs == null) ? "" : " " + fs.getPresentationString();
				action.setToolTipText("Save Struts Project" + fsn);
			} else {
				action.setToolTipText("Save Struts Project");
			}
		}

		public void run() {
			while(!stopped) {
				synchronized (monitor) {
					try { monitor.wait(); } catch (Exception e) {}
					if(stopped) return;
					if(action != null) try {
						boolean enabled = model != null && model.getRoot().isModified();
						if(action.isEnabled() != enabled) action.setEnabled(enabled);
					} catch (Exception e) {}
				}
			}			
		}
	}
	
}
