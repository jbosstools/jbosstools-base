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
package org.jboss.tools.common.model.ui.outline;

import java.util.*;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.jboss.tools.common.model.XFilteredTreeConstraint;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

public class XModelObjectContentProvider extends WorkbenchContentProvider {
	XFilteredTreeConstraint[] filters = null;
	XModelObjectCache cache;
	private Viewer viewer = null;
	
	public void setRoot(XModelObjectCache cache) {
		if(isEqualObject(cache)) return;
		this.cache = cache;
		if(viewer != null) {
			try {
				viewer.refresh();
				((TreeViewer)viewer).expandToLevel(2);
				if(cache != null) viewer.setSelection(new StructuredSelection(cache.getObject()));
			} catch (Exception e) {
			}
		}
	}
	
	private boolean isEqualObject(XModelObjectCache cache) {
		if(this.cache == null && cache == null) return true;
		if(this.cache == null || cache == null) return false;
		return (this.cache.getObject() == cache.getObject());
	}
	
	public void addFilter(XFilteredTreeConstraint filter) {
		if(filters == null) {
			filters = new XFilteredTreeConstraint[]{filter};
		} else {
			XFilteredTreeConstraint[] fs = new XFilteredTreeConstraint[filters.length + 1];
			System.arraycopy(filters, 0, fs, 0, filters.length);
			fs[filters.length] = filter;
			filters = fs;
		}
	}
	
	public Object[] getChildren(Object element) {
		if(!(element instanceof XModelObject)) return new XModelObject[0];
		XModelObject o = (XModelObject)element;
		XModelObject[] os = o.getChildren();
		if(filters == null || os.length == 0) return os;
		if(isHidingAllChildren(o)) return new XModelObject[0];
		if(!isHidingSomeChildren(o)) return os;
		ArrayList<XModelObject> l = new ArrayList<XModelObject>();
		for (int i = 0; i < os.length; i++) if(accepts(os[i])) l.add(os[i]);
		return l.toArray(new XModelObject[0]);
	}
	
	public Object[] getElements(Object element) {
		XModelObject o = cache == null ? null : cache.getObject();
		return (o == null) ? new Object[0] : new Object[]{o};		
	}

	public Object getParent(Object element) {
		if(!(element instanceof XModelObject)) return null;
		XModelObject o = (XModelObject)element;
		return o.getParent();
	}
	
	private boolean isHidingAllChildren(XModelObject o) {
		if(filters == null) return false;
		for (int i = 0; i < filters.length; i++) 
			if(filters[i].isHidingAllChildren(o)) return true;
		return false;
	}
	
	private boolean isHidingSomeChildren(XModelObject o) {
		if(filters == null) return false;
		for (int i = 0; i < filters.length; i++) 
			if(filters[i].isHidingSomeChildren(o)) return true;
		return false;
	}
	
	private boolean accepts(XModelObject o) {
		if(filters == null) return true;
		for (int i = 0; i < filters.length; i++) 
			if(filters[i].accepts(o)) return true;
		return false;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		try { 
			super.inputChanged(viewer, oldInput, newInput);
			this.viewer = viewer;
			if(viewer instanceof TreeViewer) {
				((TreeViewer)viewer).setAutoExpandLevel(2);
			}
			if(viewer.getControl() == null || viewer.getControl().isDisposed()) return;
			viewer.refresh();
			if(viewer.getSelection() == null || viewer.getSelection().isEmpty()) {
				if(cache != null) viewer.setSelection(new StructuredSelection(cache.getObject()));
			}
		} catch (Exception t) {
			//ignore
		}
	}
	
	public void dispose() {
		super.dispose();
		viewer = null;
		filters = null;
	}
}
