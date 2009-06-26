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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.IPage;
import org.eclipse.ui.views.properties.*;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.constraint.impl.XAttributeConstraintAList;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.adapter.IModelObjectAdapter;
import org.jboss.tools.common.model.event.*;

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
		XAttribute a = modelObject.getModelEntity().getAttribute(n);
		if(a != null && v != null && a.getConstraint() instanceof XAttributeConstraintAList) {
			v = WizardKeys.getVisualListValue(a, v);
		}
		cachedValues.setProperty(n, "" + v); //$NON-NLS-1$
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
			try {
				modelObject.getModel().editObjectAttribute(modelObject, n, defaultValue.toString());
			} catch (XModelException e) {
				throw new IllegalArgumentException(e);
			}
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
		cachedValues.setProperty(n, "" + value); //$NON-NLS-1$
		value = fromVisualToModel(n, value.toString());
		if(modelObject.isActive()) {
			try {
				modelObject.getModel().editObjectAttribute(modelObject, n, value.toString());
			} catch (XModelException e) {
				throw new IllegalArgumentException(e);
			}
		} else {
			modelObject.setAttributeValue(n, value.toString());
		}
		
	}

	private String fromVisualToModel(String n, String value) {
		XAttribute a = modelObject.getModelEntity().getAttribute(n);
		if(a != null && value != null && a.getConstraint() instanceof XAttributeConstraintAList) {
			String[] vs = ((XAttributeConstraintAList)a.getConstraint()).getValues();
			for (int i = 0; i < vs.length; i++) {
				String v = WizardKeys.getVisualListValue(a, vs[i]);
				if(value.equals(v)) return vs[i];
			}
		}
		return value;
	}
	// custom methods

	public void setModelObject(XModelObject object) {
		modelObject = object;
		XAttribute[] attrs = modelObject.getModelEntity().getAttributes();
		propertyDescriptors = new ArrayList<IPropertyDescriptor>();
		for (int i=0;i<attrs.length;++i) {
			if (attrs[i].isVisible() && !"element type".equals(attrs[i].getName())) { //$NON-NLS-1$
				//propertyDescriptors.add(new TextPropertyDescriptor(attrs[i].getName(),attrs[i].getName()));
				propertyDescriptors.add(new XAttributePropertyDescription(this, attrs[i], modelObject));
			}
		}
		XModel model = modelObject.getModel();
		if(model.getManager("propertySheetUpdate") == null) { //$NON-NLS-1$
			model.addModelTreeListener(listener);
			model.addManager("propertySheetUpdate", listener); //$NON-NLS-1$
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
				sh = (PropertySheet)PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.ui.views.PropertySheet"); //$NON-NLS-1$
			} catch (NullPointerException exc) {
				//ignore
			}
			if(sh == null) return;
			IPage page = sh.getCurrentPage();
			if(page instanceof PropertySheetPage) {
				PropertySheetPage p = (PropertySheetPage)page;
				if(p == null || p.getControl() == null || p.getControl().isDisposed()) return;
				p.refresh();
			}
		}
		public void structureChanged(XModelTreeEvent event) {}
	}

	public boolean isPropertyResettable(Object id) {
		String n = getAttributeNameById(id);
		return modelObject != null && modelObject.isAttributeEditable(n);
	}

}
