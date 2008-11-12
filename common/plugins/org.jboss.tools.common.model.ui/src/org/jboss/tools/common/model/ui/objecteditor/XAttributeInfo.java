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
package org.jboss.tools.common.model.ui.objecteditor;

import java.util.HashMap;
import java.util.Map;

import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyEditor;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.constraint.impl.XAttributeConstraintAList;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.*;

public class XAttributeInfo {
	XModelObject object;
	String name;
	String value;
	IPropertyEditor propertyEditor;
	
	Map<String, String> visualToModel = null;
	Map<String, String> modelToVisual = null;
	
	public XAttributeInfo(XModelObject object, String name) {
		this.object = object;
		this.name = name;
		this.value = getValue();
		
		XAttribute a = object.getModelEntity().getAttribute(name);
		visualToModel = null;
		if(a != null && a.getConstraint() instanceof XAttributeConstraintAList) {
			visualToModel = new HashMap<String, String>();
			modelToVisual = new HashMap<String, String>();
			String[] vs = ((XAttributeConstraintAList)a.getConstraint()).getValues();
			for (int i = 0; i < vs.length; i++) {
				String vv = WizardKeys.getVisualListValue(a, vs[i]);
				visualToModel.put(vv, vs[i]);
				modelToVisual.put(vs[i], vv);
			}
		}
	}
	
	public XModelObject getObject() {
		return object;
	}
	
	public String getName() {
		return name;
	}
	
	public String getValue() {
		String v = object.getAttributeValue(name);
		if(modelToVisual != null && v != null && modelToVisual.containsKey(v)) v = modelToVisual.get(v);
		return v;
	}
	
	public boolean isEditable() {
		if(!object.isAttributeEditable(name)) return false;
		XAttribute a = object.getModelEntity().getAttribute(name);
		return a != null && !a.getEditor().getName().equals("Uneditable");
	}
	
	public void setValue(String value) {
		this.value = value;
	}
	
	public void commit() throws XModelException {
		String modelValue = value;
		if(visualToModel != null && visualToModel.containsKey(value)) {
			modelValue = visualToModel.get(value);
		}
		if(propertyEditor != null && propertyEditor.getInput() instanceof IModelPropertyEditorAdapter) {
			IModelPropertyEditorAdapter adapter = (IModelPropertyEditorAdapter)propertyEditor.getInput();
			adapter.setValue(modelValue);
			adapter.store();
		} else {
			object.getModel().editObjectAttribute(object, name, modelValue);
		}
		value = getValue();
	}
	
}
