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
package org.jboss.tools.common.model.ui.texteditors;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.util.XModelTreeListenerSWTSync;

public class XMLTextEditorStandAlone extends XMLTextEditorComponent implements XModelTreeListener {
	protected XModelTreeListenerSWTSync syncListener = new XModelTreeListenerSWTSync(this);
	protected long timeStamp = -1;
	protected long lastModifiedTimeStamp = -1;
	
	public XMLTextEditorStandAlone() {
		isStandAlone = true;
	}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		super.init(site, XModelObjectEditorInput.checkInput(input));
	}
	
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		if(getModel() == null) {
			return;
		}
		XModelObject o = getModelObject();
		setObject(o);
		if(o != null) o.getModel().addModelTreeListener(syncListener);
	}

	protected void doSetInput(IEditorInput input) throws CoreException {
		super.doSetInput(XModelObjectEditorInput.checkInput(input));
	}

	public void doRevertToSaved() {
		super.doRevertToSaved();
		updateDocument();
	}

	public void dispose() {
		XModelObject o = getModelObject();
		if(o != null) o.getModel().removeModelTreeListener(syncListener);
		super.dispose();
		if(o != null && o.isModified() && o.isActive()) {
			try {
				XAction action = XActionInvoker.getAction("DiscardActions.Discard", o);
				if(action != null) {
					// to avoid confirmation
					action.executeHandler(o, null); 
				} else if(o.getParent() instanceof FolderImpl) {
					((FolderImpl)o.getParent()).discardChildFile(o);
				}
 			} catch (XModelException e) {	
 				//ignore 
 			}
		}
	}

	boolean lock2 = false;
	
	public void nodeChanged(XModelTreeEvent event) {
		if(lock2) return;
		if(needsUpdate()) {
			Display.getDefault().syncExec(new U());
		}
	}
	
	public void structureChanged(XModelTreeEvent event) {
		if(lock2) return;
		if(needsUpdate()) {
			Display.getDefault().syncExec(new U());
		}
	}
	
	class U implements Runnable {
		public void run() {
			lock2 = true;
			update0();
			while(needsUpdate()) {
				update0();
			}
			lock2 = false;
		}
	}
	
	protected boolean needsUpdate() {
		XModelObject o = getModelObject();
		if(/*o == object &&*/ (o == null || o.getTimeStamp() == timeStamp)) {
			if(o != null && o.getLastModificationTimeStamp() != lastModifiedTimeStamp) {
				if(!o.isModified()) lastModifiedTimeStamp = o.getLastModificationTimeStamp();
				firePropertyChange(IEditorPart.PROP_DIRTY);
				updateModification();
			}
			return false;
		} 
//		object = o;
		timeStamp = (o == null) ? -1 : o.getTimeStamp();
		return true;
	}

	public void update0() {
		updateDocument();
	}
}
