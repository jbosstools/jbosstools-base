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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.util.Assert;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.extension.ExtensionPointUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class AdapterFactory {
	public static final String ATTRIBUTE_ADAPTER_EXT_POINT = "org.jboss.tools.common.model.ui.attributeAdapter";
	private static Map<String,Class> classes = new HashMap<String,Class>();

	public AdapterFactory() {}
	
	public static IModelPropertyEditorAdapter getAdapter(XAttribute attribute, XModelObject modelObject, XModel model) {
		return getAdapter(attribute, modelObject, null, model);		
	}
	
	public static IModelPropertyEditorAdapter getAdapter(XAttribute attribute, XAttributeData attributeData, XModel model) {
		Assert.isTrue(model != null, "AdapterFactory.getAdapter(XAttribute, XAttributeData, XModel) model cannot be null!");
		return getAdapter(attribute, null, attributeData, model);		
	}
	
	public static IModelPropertyEditorAdapter getAdapter(XAttribute attribute, XModelObject modelObject, XAttributeData attributeData, XModel model) {
		IModelPropertyEditorAdapter adapter;
		try {
			adapter = (IModelPropertyEditorAdapter)getAdapterClass(attribute).newInstance();
		} catch (Exception e) {
			adapter = new DefaultValueAdapter(); 
		}
		if (model == null && modelObject != null) model = modelObject.getModel();
		adapter.setModel(model);
		adapter.setAttribute(attribute);
		if(modelObject != null) adapter.setModelObject(modelObject);
		if(attributeData != null) adapter.setAttributeData(attributeData);
		adapter.load();
		return adapter;
	}
	
	private static Class getAdapterClass(XAttribute attribute) {
		return getAdapterClass(attribute.getEditor().getName());
	}
	
	private static Class getAdapterClass(String id) {
		Class c = classes.get(id);
		if(c != null) return c;
		try {
			c = ExtensionPointUtil.findClassByElementId(ATTRIBUTE_ADAPTER_EXT_POINT, id).getClass();			
		} catch (Exception e) {
			if(ModelUIPlugin.getDefault().isDebugging()) {
				ModelUIPlugin.log("Default adapter for " + id);
			}
			c = DefaultValueAdapter.class;
		}
		classes.put(id, c);		
		return c;
	}

}
