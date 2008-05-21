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

import org.eclipse.jface.viewers.Viewer;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintL;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.XAttributePropertyDescription;

public class DefaultXAttributeListContentProvider implements IListContentProvider {
	protected XAttribute attribute;
	protected String[] tags = new String[0];
	
	public DefaultXAttributeListContentProvider() {}
	
	public void setAttribute(XAttribute attribute) {
		this.attribute = attribute;
	}
	
	protected void loadTags() {
		tags = (attribute != null && attribute.getConstraint() instanceof XAttributeConstraintL) 
			? ((XAttributeConstraintL)attribute.getConstraint()).getValues()
			: new String[0];
	}
	
	public void dispose() {}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof XAttributePropertyDescription) {
			XAttribute attribute = ((XAttributePropertyDescription)inputElement).getAttribute();
			setAttribute(attribute);
		}
		loadTags();
		return tags; 
	}

}
