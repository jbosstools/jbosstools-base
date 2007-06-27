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
package org.jboss.tools.common.model.ui.navigator;

import org.eclipse.swt.widgets.*;
import org.eclipse.jface.viewers.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.event.*;

public class TreeViewerModelListenerImpl implements XModelTreeListener {
	protected TreeViewer viewer;
	protected XModelObject root;
	
	public void setViewer(TreeViewer viewer) {
		this.viewer = viewer;
		root = getRoot();
	}
	
	protected XModelObject getRoot() {
		Object i = viewer.getInput();
		return (i instanceof XModelObjectCache) ? ((XModelObjectCache)i).getObject() 
		: (i instanceof XModelObject) ? (XModelObject)i : null;
	}
	
	public void nodeChanged(XModelTreeEvent event) {
		if (event == null) return;
		updateNode(event.getModelObject());
	}
	
	public void updateNode(XModelObject object) {
		if (viewer == null) return;
		Control ctrl = viewer.getControl();
		if (ctrl == null || ctrl.isDisposed()) return;
		viewer.update(object, null);
	}
	
	protected XModelObject getSelectedObject() {
		ISelection selection = viewer.getSelection();
		if(!(selection instanceof IStructuredSelection) || selection.isEmpty()) return null;
		IStructuredSelection ss = (IStructuredSelection)selection;
		Object o = ss.getFirstElement();
		return (!(o instanceof XModelObject)) ? null : (XModelObject)o;
	}
	
	static int reportRefreshProblemCount = 0;
	
	public void structureChanged(XModelTreeEvent event) {
		if (viewer == null || event == null) return;
		Control ctrl = viewer.getControl();
		if (ctrl == null || ctrl.isDisposed()) return;
		XModelObject r = getRoot();
		if(root != r) {
			root = r;
			try { viewer.refresh(); } catch (Exception e) { /* ignore */}
			return;					
		}
		XModelObject selected = getSelectedObject();
		XModelObject refreshObject = event.getModelObject();
		if(refreshObject != null) {
			try {
				if(isFileParent(refreshObject, r)) {
					viewer.refresh(r);
				} else {
					viewer.refresh(refreshObject);
				}
			} catch (Exception e) {
				if(reportRefreshProblemCount == 0) {
					reportRefreshProblemCount = 1;
					String message = "Cannot refresh tree for " + refreshObject.getPresentationString();
					ModelUIPlugin.getPluginLog().logError( message, e);
				}
				return;
			}			
		}
		if(event.kind() == XModelTreeEvent.CHILD_ADDED) {
//			XModelObject c = (XModelObject)event.getInfo();
				//Attention! wizard shall decide when select created object
				// but files may be in tree children of later updated nodes
//				if(c.getParent() == selected && hasChild(selected, c)) {
//					setSelection(c);
//				}
		} else if(event.kind() == XModelTreeEvent.CHILD_REMOVED) {
			if(selected != null && !selected.isActive() && 
			event.getInfo().toString().endsWith("/" + selected.getPathPart())) {
				setSelection(event.getModelObject());
			}
		}
	}
	
	boolean isFileParent(XModelObject refreshObject, XModelObject r) {
		if(refreshObject == null || refreshObject.getFileType() != XModelObject.FILE) return false;
		while(r != null && r != refreshObject) r = r.getParent();
		return r == refreshObject;		
	}
	
	protected void setSelection(XModelObject o) {
		if(viewer.getControl() == null || viewer.getControl().isDisposed()) return;
		viewer.setSelection(new StructuredSelection(o), true);
	}
	
	//never invoked
	boolean hasChild(XModelObject p, XModelObject c) {
		ITreeContentProvider cp = (ITreeContentProvider)viewer.getContentProvider();
		Object[] os = cp.getChildren(p);
		if(os == null || os.length == 0) return false;
		for (int i = 0; i < os.length; i++) {
			if(os[i] == c) return true;			
		}
		return false;		
	}
	
	public void disopse() {
		viewer = null;
		// try disconnect from model
		if (root!=null) {
			XModel model = root.getModel();
			if (model!=null) {
				model.removeModelTreeListener(this);
			}
		}
	}
}
