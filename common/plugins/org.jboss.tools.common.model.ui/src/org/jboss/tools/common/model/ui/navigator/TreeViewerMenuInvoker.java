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

import java.util.ArrayList;

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.action.XMenuInvoker;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.TreeItem;

import org.jboss.tools.common.model.XModelObject;

public class TreeViewerMenuInvoker extends XMenuInvoker {

	public XModelObject getSelectedModelObject() {
		TreeItem[] ti = ((TreeViewer)viewer).getTree().getSelection();
		return (ti == null || ti.length == 0) ? null : getObjectByItem(ti[0]);
	}
	
	public XModelObject[] getSelectedModelObjects() {
		TreeItem[] ti = ((TreeViewer)viewer).getTree().getSelection();
		if(ti == null || ti.length < 2) return null;
		ArrayList<XModelObject> l = new ArrayList<XModelObject>();
		for (int i = 0; i < ti.length; i++) {
			XModelObject o = getObjectByItem(ti[i]);
			if(o != null) l.add(o);
		}
		XModelObject[] res = l.toArray(new XModelObject[0]); 
		return (res.length == 0) ? null : res; 
	}
	
	private XModelObject getObjectByItem(TreeItem i) {
		Object data = i.getData();
		if(data instanceof XModelObject) return (XModelObject)data;
		if(data instanceof IAdaptable) return (XModelObject)((IAdaptable)data).getAdapter(XModelObject.class);
		return null;
	}
	
	public XModelObject getModelObjectAt(Point p) {
		TreeItem i = ((TreeViewer)viewer).getTree().getItem(p);
		return (i == null) ? null : getObjectByItem(i);
	}
	
}
