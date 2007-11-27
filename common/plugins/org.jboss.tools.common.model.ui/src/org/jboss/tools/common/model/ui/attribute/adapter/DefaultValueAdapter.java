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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyEditor;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.markers.XMarkerManager;

public class DefaultValueAdapter implements IModelPropertyEditorAdapter, IAdaptable {

	protected Object value = "";
	protected PropertyChangeSupport pcs = new PropertyChangeSupport(this);
	
	protected XModel model; 
	protected XAttribute attribute; 
	protected XModelObject modelObject;
	protected XAttributeData attributeData;
	
	protected static final int UNKNOW_TARGET = -1; 
	protected static final int MODELOBJECT_TARGET = 0; 
	protected static final int ATTRIBUTEDATA_TARGET = 1; 
	protected int storeTarget = UNKNOW_TARGET;
	
	protected boolean autoStore = true;
	protected boolean storeLocked = false;

	public DefaultValueAdapter() {}

	public void store() {
		if(isStoreLocked()) return;
		if (MODELOBJECT_TARGET == this.storeTarget) {
			String v = getStringValue(true);
			if(v != null && attribute.isTrimmable()) v = v.trim();
			String n = attribute.getName();
			if(modelObject.isActive()) {
				modelObject.getModel().editObjectAttribute(modelObject, n, v);
			} else {
				modelObject.setAttributeValue(attribute.getName(), getValue().toString());
			}
			String v1 = modelObject.getAttributeValue(n);
			if(!v1.equals(v)) {
				setValue(v1);
				fireValueChange(v, v1);					
			}
		} else if(attributeData!=null) {
				attributeData.setValue(getStringValue(true));
		}
//		storeValue();
	}

	public void load() {
		if (MODELOBJECT_TARGET == storeTarget) {
			this.setValue(modelObject.getAttributeValue(attribute.getName()));
		} else {
			this.setValue(attributeData.getValue());
		}
//		loadValue();
	}

	public void setValue(Object value) {
		if(this.value == value || (this.value != null && this.value.equals(value))) return;
		Object oldValue = this.value;
		this.value = value;
		fireValueChange(oldValue, this.value);
	}
	public void fireValueChange(Object oldValue, Object newValue) {
		if(pcs != null) pcs.firePropertyChange(IPropertyEditor.VALUE, oldValue, newValue);
	}
	

	// IValueProvider
	public void addValueChangeListener(PropertyChangeListener l) {
		if (pcs!=null) pcs.addPropertyChangeListener(l);
	}
	public Object getValue() {
		return this.value;
	}
	public void removeValueChangeListener(PropertyChangeListener l) {
		if (pcs!=null) pcs.removePropertyChangeListener(l);
	}
	
	// IValueChangeListener
	public void valueChange(PropertyChangeEvent event) {
		setValue(event.getNewValue());
		// store to model for Slava
		if (isAutoStore()) store();
	}
	
	// IAdaptable
	public Object getAdapter(Class adapter) {
		if (adapter == IValueProvider.class) {
			return this;
		}
		if (adapter == IValueChangeListener.class) {
			return this;
		}
		if(adapter == IAttributeErrorProvider.class) {
			return this;
		}
		return null;
	}

	public XAttribute getAttribute() {
		return attribute;
	}
	
	public void setAttribute(XAttribute attribute) {
		this.attribute = attribute;
	}

	public XAttributeData getAttributeData() {
		return attributeData;
	}
	
	public void setAttributeData(XAttributeData data) {
		attributeData = data;
		this.storeTarget = ATTRIBUTEDATA_TARGET;
	}

	public XModelObject getModelObject() {
		return modelObject;
	}
	
	public void setModelObject(XModelObject object) {
		modelObject = object;
		storeTarget = MODELOBJECT_TARGET;
	}

	public boolean isAutoStore() {
		return autoStore;
	}
	
	public void setAutoStore(boolean b) {
		autoStore = b;
	}

	public boolean isStoreLocked() {
		return storeLocked;
	}
	
	public void setStoreLocked(boolean b) {
		storeLocked = b;
	}

	public XModel getModel() {
		return model;
	}
	
	public void setModel(XModel model) {
		this.model = model;
	}

	public String getStringValue(boolean returnNullAsEmptyString) {
		Object o = getValue();
		return (o != null) ? o.toString() : (returnNullAsEmptyString) ? "" : null;
	}

	public void dispose() {
		pcs = null;
	}

	public boolean hasErrors() {
		if(ATTRIBUTEDATA_TARGET == storeTarget) return false;
		return attribute != null && XMarkerManager.getInstance().hasErrors(modelObject, attribute.getName());
	}

	public String getError() {
		if(modelObject == null || attribute == null) return null;
		return XMarkerManager.getInstance().getError(modelObject, attribute.getName());
	}
}
