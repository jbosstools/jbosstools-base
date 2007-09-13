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

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

public class OutlineContentProvider extends XModelObjectContentProvider {	
	XFilteredTree tree;
	boolean isProjectEnabled = false;
	
	public void dispose() {
		super.dispose();
		if (tree!=null) tree.dispose();
		tree = null;
	}
	
	public void setRoot(XModelObjectCache cache) {
		super.setRoot(cache);
///		createFilteredTree();
	}
	
	public Object[] getElements(Object element) {
		Object[] es = super.getElements(element);
		if(tree == null) return es;
		Object[] es2 = new Object[es.length + 1];
		System.arraycopy(es, 0, es2, 0, es.length);
		es2[es.length] = tree.getRoot();
		return es2;		
	}

	public Object[] getChildren(Object parentElement) {
		Object result[] = null;
		if(cache.getObject() == parentElement || tree == null)
			return super.getChildren(parentElement);		
		if (parentElement instanceof XModelObject) {
			result = (tree == null) ? null : tree.getChildren((XModelObject)parentElement);
		}			
		return result;
	}
	
	public Object getParent(Object element)	{
		if(cache.getObject() == element || tree == null)
			return super.getParent(element);		
		if (element instanceof XModelObject) {
			return (tree == null) ? null : tree.getParent((XModelObject)element);
		}
		return null;
	}
	
	public boolean hasChildren(Object element) {
		if(cache.getObject() == element || tree == null)
			return super.hasChildren(element);		
		if (element instanceof XModelObject) {
			XModelObject o = (XModelObject)element;
			if(!o.isActive()) return false;
				return tree.hasChildren((XModelObject)element);
		}
		return false;
	}


}
