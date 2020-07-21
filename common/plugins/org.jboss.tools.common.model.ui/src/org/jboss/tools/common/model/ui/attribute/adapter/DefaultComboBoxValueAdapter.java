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

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.ILabelProvider;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.actions.IActionProvider;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;

public class DefaultComboBoxValueAdapter extends DefaultValueAdapter {
	protected ILabelProvider labelProvider;
	protected IListContentProvider listContentProvider;

	public DefaultComboBoxValueAdapter() {
	}

	protected IListContentProvider createListContentProvider(XAttribute attribute) {
		DefaultXAttributeListContentProvider p = new DefaultXAttributeListContentProvider();
		p.setAttribute(attribute);
		return p;	
	}

	public void dispose() {
		super.dispose();
		if (labelProvider!=null) labelProvider = null;
		labelProvider = null;
		if (listContentProvider!=null) listContentProvider.dispose();
		listContentProvider = null;
	}
	
	// IAdaptable
	public Object getAdapter(Class adapter) {
		if (adapter == IValueProvider.class) {
			return this;
		}
		if (adapter == IValueChangeListener.class) {
			return this;
		}
		if (adapter == IAttributeErrorProvider.class) return this;
		if (adapter == ILabelProvider.class) {
			if (this.labelProvider==null) {
				this.labelProvider = new KeyLabelProvider();
			}
			return this.labelProvider;
		}
		if (adapter == IListContentProvider.class) {
			if (this.listContentProvider==null) {
				this.listContentProvider = createListContentProvider(attribute);
			}
			return this.listContentProvider;
		}
		if (adapter == IActionProvider.class) {
			return getActionProvider();
		}
		Assert.isTrue(true, "DefaultValueAdapter instance itself cannot provide adapter for "+adapter.getName()); //$NON-NLS-1$
		return null;
	}
	
	public void setListContentProvider(IListContentProvider listContentProvider)
	{
		this.listContentProvider = listContentProvider;
	}

	class KeyLabelProvider extends DefaultXModelObjectLabelProvider {

		public String getText(Object element) {
			if(element != null && !(element instanceof XModelObject)) {
				XAttribute a = attribute;
				if(a == null && attributeData != null) a = attributeData.getAttribute();
				if(a != null) {
					String v = element.toString();
					return WizardKeys.getVisualListValue(a, v);
				}
			}
			return super.getText(element);
		}

	}
}
