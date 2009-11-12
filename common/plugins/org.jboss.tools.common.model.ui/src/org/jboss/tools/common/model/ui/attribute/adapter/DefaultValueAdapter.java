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
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.IValueFilter;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyEditor;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.markers.XMarkerManager;

public class DefaultValueAdapter implements IModelPropertyEditorAdapter, IAdaptable {

	protected Object value = ""; //$NON-NLS-1$
	protected PropertyChangeSupport pcs = new PropertyChangeSupport(this);
	protected PropertyChangeSupport pcs2 = new PropertyChangeSupport(this);
	
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

	protected String invalidValue = null;
	protected String lastCorrectValue = null;
	protected String currentError = null;

	boolean valueFilterChecked = false;
	IValueFilter valueFilter = null;

	public DefaultValueAdapter() {}

	public void store() {
		if(isStoreLocked()) return;
		if (MODELOBJECT_TARGET == this.storeTarget) {
			String v = getStringValue(true);
			if(v != null && attribute.isTrimmable()) v = v.trim();
			String n = attribute.getName();
			if(modelObject.isActive()) {

				currentError = ((XModelImpl)modelObject.getModel()).getError(modelObject, n, v);
				
				if(currentError != null) {
					invalidValue = getStringValue(true);
					lastCorrectValue = modelObject.getAttributeValue(n);
					if(pcs2 != null) {
						pcs2.firePropertyChange(IPropertyEditor.ERROR, Boolean.FALSE, Boolean.TRUE);
					}
					fireValueChange(v, v);
					return;
				} else {
					boolean changed = invalidValue != null;
					invalidValue = null;
					lastCorrectValue = null;
					if(changed && pcs2 != null) {
						pcs2.firePropertyChange(IPropertyEditor.ERROR, Boolean.TRUE, Boolean.FALSE);
					}
				}

				try {
					modelObject.getModel().editObjectAttribute(modelObject, n, v);
				} catch (XModelException e) {
					throw new IllegalArgumentException(e);
				}
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
			String value = modelObject.getAttributeValue(attribute.getName());
			if(currentError != null && invalidValue != null) {
				if(value != null && value.equals(lastCorrectValue)) {
					return;
				}
			}
			this.setValue(value);
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

	public void fireEvent(String propertyName, Object oldValue, Object newValue) {
		if(pcs != null) pcs.firePropertyChange(propertyName, oldValue, newValue);
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

	public void addErrorStateListener(PropertyChangeListener l) {
		if (pcs2!=null) pcs2.addPropertyChangeListener(l);
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
		if(adapter == IValueFilter.class) {
			if(valueFilterChecked) return valueFilter;
			valueFilterChecked = true;
			if(valueFilter == null) {
				XAttribute a = (attribute != null) ? attribute : attributeData != null ? attributeData.getAttribute() : null;
				if(a == null) return null;
				String cls = a.getProperty("valueFilter");
				if(cls == null || cls.length() == 0) return null;
				try {
					valueFilter = (IValueFilter)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
				} catch (ClassCastException exc) {
					ModelUIPlugin.getPluginLog().logError(exc);
				}
				if(valueFilter != null) {
					if(!valueFilter.init(getModelObject(), a)) {
						valueFilter = null;
					}
				}
				return valueFilter;
			}
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
		return (o != null) ? o.toString() : (returnNullAsEmptyString) ? "" : null; //$NON-NLS-1$
	}

	public void dispose() {
		pcs = null;
	}

	public boolean hasErrors() {
		if(ATTRIBUTEDATA_TARGET == storeTarget) return false;
		if(invalidValue != null && currentError != null) {
			return true;
		}
		return attribute != null && XMarkerManager.getInstance().hasErrors(modelObject, attribute.getName());
	}

	public String getError() {
		if(modelObject == null || attribute == null) return null;
		if(ATTRIBUTEDATA_TARGET != storeTarget) {
			if(invalidValue != null && currentError != null) {
				return currentError;
			}
		}
		return XMarkerManager.getInstance().getError(modelObject, attribute.getName());
	}
}
