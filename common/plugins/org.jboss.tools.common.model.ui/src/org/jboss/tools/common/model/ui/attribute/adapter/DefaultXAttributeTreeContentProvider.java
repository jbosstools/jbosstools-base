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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.Properties;

import org.jboss.tools.common.model.ui.attribute.XAttributePropertyDescription;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintL;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintT;
import org.jboss.tools.common.model.XFilteredTree;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class DefaultXAttributeTreeContentProvider implements ITreeContentProvider {

	protected XAttribute attribute;
	protected XFilteredTree filteredTree;
	protected XModelObject object;
	protected XModel model;
	protected Properties properties = new Properties();

	public DefaultXAttributeTreeContentProvider(XAttribute attribute, XModel model, XModelObject object) {
		this.attribute = attribute;
		this.model = model;
		this.object = object;
		
		if (attribute != null && attribute.getConstraint() instanceof XAttributeConstraintT) {
			XAttributeConstraintT attributeConstraint = (XAttributeConstraintT)attribute.getConstraint();
			properties = attributeConstraint.getProperties();
		}		
	}

	public Object[] getChildren(Object parentElement) {
		Object result[] = new Object[0];
		if (parentElement instanceof XModelObject) {
			result = getFilteredTree().getChildren((XModelObject)parentElement);
		} else if (getFilteredTree()!=null) {
			result = getFilteredTree().getChildren(getFilteredTree().getRoot());
		}
		return result;
	}

	public Object getParent(Object element) {
		Object result = null;
		if (element instanceof XModelObject)
			result = getFilteredTree().getParent((XModelObject)element);
		return result;
	}

	public boolean hasChildren(Object element) {
		boolean result = false;
		if (element instanceof XModelObject)
			result = getFilteredTree().hasChildren((XModelObject)element);
		return result;
	}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof XAttributePropertyDescription) {
			XAttribute attribute = ((XAttributePropertyDescription)inputElement).getAttribute();
			if (attribute != null && attribute.getConstraint() instanceof XAttributeConstraintT) {
				XAttributeConstraintT attributeConstraint = (XAttributeConstraintT)attribute.getConstraint();
				//if("no".equals(attributeConstraint.getProperties().getProperty("single")))
				//single = false;
				String filteredTreeName = attributeConstraint.getFilteredTreeName();
				
				filteredTree = this.createFilteredTree(filteredTreeName);
				return getChildren(filteredTree.getRoot());
			}
		} else if (attribute!=null) {
			if (attribute != null && attribute.getConstraint() instanceof XAttributeConstraintT) {
				XAttributeConstraintT attributeConstraint = (XAttributeConstraintT)attribute.getConstraint();
				//if("no".equals(attributeConstraint.getProperties().getProperty("single")))
				//single = false;
				String filteredTreeName = attributeConstraint.getFilteredTreeName();
				filteredTree = this.createFilteredTree(filteredTreeName);
				boolean hideRoot = properties != null && "true".equals(properties.get("hideRoot"));
				if(hideRoot) {
					return getChildren(filteredTree.getRoot());
				} else {
					return new Object[] { filteredTree.getRoot() };
				}
			} else if (attribute.getConstraint() instanceof XAttributeConstraintL) {
				return ((XAttributeConstraintL)attribute.getConstraint()).getValues();
			}
		}
		return null;
	}

	public void dispose() {
///		properties = null;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		try { viewer.refresh(); } catch (Exception e) {}
	}

	protected XFilteredTree createFilteredTree(String filteredTreeName) {
		String classname = model.getMetaData().getMapping("FilteredTrees").getValue(filteredTreeName);
		try {
			XFilteredTree tree = (XFilteredTree)ModelFeatureFactory.getInstance().createFeatureInstance(classname);
			tree.setModel(model);
			tree.setConstraint(new Object[]{attribute, object});
			return tree;
		} catch(Exception exception) {
			return null;
		}
	}

	public XFilteredTree getFilteredTree() {
		return filteredTree;
	}
	public void setFilteredTree(XFilteredTree tree) {
		filteredTree = tree;
	}

	/**
	 * @return
	 */
	public Properties getProperties() {
		return properties;
	}

}
