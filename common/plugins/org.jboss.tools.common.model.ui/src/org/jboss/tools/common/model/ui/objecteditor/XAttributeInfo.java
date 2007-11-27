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

import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyEditor;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;

public class XAttributeInfo {
	XModelObject object;
	String name;
	String value;
	IPropertyEditor propertyEditor;
	
	public XAttributeInfo(XModelObject object, String name) {
		this.object = object;
		this.name = name;
		this.value = getValue();
	}
	
	public XModelObject getObject() {
		return object;
	}
	
	public String getName() {
		return name;
	}
	
	public String getValue() {
		return object.getAttributeValue(name);
	}
	
	public boolean isEditable() {
		if(!object.isAttributeEditable(name)) return false;
		XAttribute a = object.getModelEntity().getAttribute(name);
		return a != null && !a.getEditor().getName().equals("Uneditable");
	}
	
	public void setValue(String value) {
		this.value = value; 
	}
	
	public void commit() {
		if(propertyEditor != null && propertyEditor.getInput() instanceof IModelPropertyEditorAdapter) {
			IModelPropertyEditorAdapter adapter = (IModelPropertyEditorAdapter)propertyEditor.getInput();
			adapter.setValue(this.value);
			adapter.store();
		} else {
			object.getModel().editObjectAttribute(object, name, value);
		}
		value = getValue();
	}
	
}
