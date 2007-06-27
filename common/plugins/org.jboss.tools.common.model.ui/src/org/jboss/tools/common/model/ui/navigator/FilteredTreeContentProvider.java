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

import org.eclipse.jface.viewers.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class FilteredTreeContentProvider implements ITreeContentProvider {
	protected String filteredTreeName;
	protected XFilteredTree filteredTree;
//	private TreeViewer viewer = null;
	private XModel model;
	
	public void setViewer(TreeViewer viewer) {
//		this.viewer = viewer;	
	}
	
	public void setModel(XModel model) {
		this.model = model;
	}
	
	public void setFilteredTreeName(String filteredTreeName) {
		this.filteredTreeName = filteredTreeName;
	}
	
	protected XFilteredTree createFilteredTree()	{
		XFilteredTree result = getFilteredTree();
		if(result != null && (result.getRoot() == null || !result.getRoot().isActive())) {
			 result = null;		
		}
		if (result == null) {			
			try	{
				String classname = model.getMetaData().getMapping("FilteredTrees").getValue(filteredTreeName);
				result = (XFilteredTree)getClass().getClassLoader().loadClass(classname).newInstance();
				result.setModel(model);
			} catch(Exception ex) {
				ModelUIPlugin.getPluginLog().logError(ex);
			}
		}		
		return result;
	}

	protected XFilteredTree getFilteredTree() {
		return filteredTree;
	}
	
	public Object[] getChildren(Object parentElement) {
		return getFilteredTree().getChildren((XModelObject)parentElement);			
	}
	
	public Object getParent(Object element)	{
		return getFilteredTree().getParent((XModelObject)element);
	}
	
	public boolean hasChildren(Object element) {
		return getFilteredTree().hasChildren((XModelObject)element);
	}
	
	public Object[] getElements(Object inputElement) {
		filteredTree = createFilteredTree();		
		return (filteredTree == null) ? null : new Object[]{filteredTree.getRoot()};
	}
	
	public void dispose() {
//		viewer = null;
		filteredTree = null;
	}
	
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {}
	
}
