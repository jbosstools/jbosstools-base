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
package org.jboss.tools.common.model.ui.attribute;

import org.jboss.tools.common.model.ui.*;
import org.jboss.tools.common.model.ui.attribute.adapter.*;
import org.jboss.tools.common.model.ui.attribute.editor.*;
import org.jboss.tools.common.model.ui.objecteditor.XTable;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.*;

public class XAttributePropertyDescription implements IPropertyDescriptorEx, IXModelSupport {
	
	protected XAttribute attribute;
	protected XModelObject modelObject;
	protected XAttributeData attributeData;
	protected PropertyEditor propertyEditor;
	protected IPropertySource propertySource;

	protected XAttributePropertyDescription() {}
	
	public XAttributePropertyDescription(IPropertySource propertySource, XAttribute attribute, XModelObject modelObject) {
		this.propertySource = propertySource;
		this.attribute = attribute;
		this.modelObject = modelObject;
		Object adapter = AdapterFactory.getAdapter(attribute, modelObject, modelObject.getModel());		
		this.propertyEditor = PropertyEditorFactory.createPropertyEditor(adapter, attribute, modelObject);
	}

	public XAttributePropertyDescription(IPropertySource propertySource, XAttribute attribute, XAttributeData attributeData, XModel model) {
		this.propertySource = propertySource;
		this.attribute = attribute;
		this.attributeData = attributeData;
		Object adapter = AdapterFactory.getAdapter(attribute, attributeData, model);		
		this.propertyEditor = PropertyEditorFactory.createPropertyEditor(adapter, attribute, attributeData);
	}

	// PropertySource
	public IPropertySource getPropertySource() {
		return this.propertySource;
	}

	// CellEditor
	public CellEditor createPropertyEditor(Composite parent) {
		return (!isEditable()) ? null : propertyEditor.getCellEditor(parent);
	}
	
	// FieldEditor
	public void createFieldEditor(Composite parent) {
		propertyEditor.getFieldEditor(parent);
	}

	public String getCategory() {
		return null;//"General";
	}

	public String getDescription() {
		return attribute.getDisplayName();
	}

	public String getDisplayName() {
		return attribute.getName();
	}

	public String[] getFilterFlags() {
		return null;
	}

	public Object getHelpContextIds() {
		return null;
	}

	public Object getId() {
		return attribute.getName();
	}
	
	ILabelProvider label;

	public ILabelProvider getLabelProvider() {
		if(label == null) label = new LP();
		return label;
	}
	
	class LP extends LabelProvider {
		public String getText(Object element) {
			Object value = getPropertySource().getPropertyValue(getId());
			if(value == null) return null;
			String s = value.toString();
			s = XTable.toVisualValue(s); //XModelObjectLoaderUtil.saveToXMLAttribute(s);
			return s;
		}

		public Image getImage(Object element) {
			///if(attribute == null || modelObject == null) return null;
			///boolean b = XMarkerManager.getInstance().hasErrors(modelObject, attribute.getName());
			///return (!b) ? null : LabelDecoratorImpl.errorImage;
			return null;
		}
	}

	public boolean isCompatibleWith(IPropertyDescriptor anotherProperty) {
		return false;
	}

	public Object getAdapter(Class adapter) {
		if (adapter == ILabelProvider.class) {
			return new DefaultXModelObjectLabelProvider();
		}
		if (adapter == ITreeContentProvider.class) {
			return new DefaultXAttributeTreeContentProvider(getAttribute(), getModel(), modelObject);
		}
		if (adapter == IListContentProvider.class) {
			DefaultXAttributeListContentProvider p = new DefaultXAttributeListContentProvider();
			p.setAttribute(getAttribute());
			return p;
		}
		if (adapter == INewContentProvider.class) {
			return null;
		}
		if (adapter == ISelectionProvider.class) {
			return null;
		}
		if (adapter == ISelectionChangedListener.class) {
			return null;
		}
		if (adapter == IValueProvider.class) {
			return null;
		}
		if (adapter == IValueChangeListener.class) {
			return null;
		}
		return null;
	}


	public XModel getModel() {
		return ((IXModelSupport)getPropertySource()).getModel();
	}

	public PropertyEditor getPropertyEditor() {
		return (!isEditable()) ? null : propertyEditor;
	}

	public XAttribute getAttribute() {
		return attribute;
	}

	public void setAttribute(XAttribute attribute) {
		this.attribute = attribute;
	}
	
	protected boolean isEditable() {
		return (modelObject == null || modelObject.isAttributeEditable(attribute.getName()));
	}

}
