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

import java.util.*;

import org.jboss.tools.common.model.util.XModelTreeListenerSWTASync;
import org.eclipse.ui.views.properties.*;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.adapter.IModelObjectAdapter;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class XModelObjectPropertySource implements IPropertySource, IXModelSupport, IModelObjectAdapter, IPropertySource2 {
	protected ArrayList<IPropertyDescriptor> propertyDescriptors;
	protected XModelObject modelObject;
	protected Properties cachedValues = new Properties();
	private XModelTreeListener listener = new XModelTreeListenerSWTASync(new XModelTreeListenerImpl());

	public XModelObjectPropertySource() {}
	
	public void dispose() {
		if (modelObject!=null && modelObject.getModel()!=null) {
			modelObject.getModel().removeModelTreeListener(listener);
			listener = null;
		}
	}

	public Object getEditableValue() {
		return null;
	}

	public IPropertyDescriptor[] getPropertyDescriptors() {
		return (IPropertyDescriptor[])propertyDescriptors.toArray(new IPropertyDescriptor[propertyDescriptors.size()]);
	}

	public Object getPropertyValue(Object id) {
		String n = getAttributeNameById(id);
		String v = modelObject.getAttributeValue(n);
		cachedValues.setProperty(n, "" + v);
		return v;
	}

	public boolean isPropertySet(Object id) {
		String n = getAttributeNameById(id);
		String defaultValue = modelObject.getModelEntity().getAttribute(n).getDefaultValue();
		return !getPropertyValue(id).equals(defaultValue);
	}

	public void resetPropertyValue(Object id) {
		String n = getAttributeNameById(id);
		String defaultValue = modelObject.getModelEntity().getAttribute(n).getDefaultValue();
		if(defaultValue == null) return;
		if(modelObject.isActive()) {
			modelObject.getModel().editObjectAttribute(modelObject, n, defaultValue.toString());
		} else {
			modelObject.setAttributeValue(n, defaultValue.toString());
		}
	}
	
	private String getAttributeNameById(Object id) {
		return (id == null) ? null : id.toString();
	}

	public void setPropertyValue(Object id, Object value) {
		if(value == null) return;
		String n = getAttributeNameById(id);
		String v = cachedValues.getProperty(n);
		if(value.equals(v)) return;
		cachedValues.setProperty(n, "" + value);
		if(modelObject.isActive()) {
			modelObject.getModel().editObjectAttribute(modelObject, n, value.toString());
		} else {
			modelObject.setAttributeValue(n, value.toString());
		}
		
	}

	// custom methods

	public void setModelObject(XModelObject object) {
		modelObject = object;
		XAttribute[] attrs = modelObject.getModelEntity().getAttributes();
		propertyDescriptors = new ArrayList<IPropertyDescriptor>();
		for (int i=0;i<attrs.length;++i) {
			if (attrs[i].isVisible() && !"element type".equals(attrs[i].getName())) {
				//propertyDescriptors.add(new TextPropertyDescriptor(attrs[i].getName(),attrs[i].getName()));
				propertyDescriptors.add(new XAttributePropertyDescription(this, attrs[i], modelObject));
			}
		}
		XModel model = modelObject.getModel();
		if(model.getManager("propertySheetUpdate") == null) {
			model.addModelTreeListener(listener);
			model.addManager("propertySheetUpdate", listener);
		}
	}

	public XModel getModel() {
		return modelObject.getModel();
	}
	
	class XModelTreeListenerImpl implements XModelTreeListener {
		public void nodeChanged(XModelTreeEvent event) {
			if(modelObject == null || modelObject != event.getModelObject()) return;
			PropertySheet sh = null;
			try { 
				sh = (PropertySheet)ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.ui.views.PropertySheet");
			} catch (Exception exc) {
				//ignore
			}
			if(sh == null) return;
			try {
				PropertySheetPage p = (PropertySheetPage)sh.getCurrentPage();
				if(p != null) p.refresh();
			} catch (Exception e) {
				//ignore
			}
		}
		public void structureChanged(XModelTreeEvent event) {}
	}

	public boolean isPropertyResettable(Object id) {
		String n = getAttributeNameById(id);
		return modelObject != null && modelObject.isAttributeEditable(n);
	}

}
