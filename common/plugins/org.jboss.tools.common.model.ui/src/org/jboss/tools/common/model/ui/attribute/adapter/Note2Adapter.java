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

import java.util.Properties;

import org.jboss.tools.common.model.ui.IActionHelper;
import org.eclipse.swt.widgets.*;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;

public class Note2Adapter extends DefaultValueAdapter implements IActionHelper {
	
	public Object getAdapter(Class adapter) {
		if(adapter == IActionHelper.class) return this;
		return super.getAdapter(adapter);
	}
	
	public void setValue(Object value) {
		String s = (value == null) ? "" : value.toString();
		super.setValue(decode(s));
	}
	
	protected String decode(String text) {
		return text; ///XModelObjectLoaderUtil.loadFromXMLAttribute(text);
	}

	protected String encode(String text) {
		return text;///XModelObjectLoaderUtil.saveToXMLAttribute(text);
	}

	public void store() {
		if(isStoreLocked()) return;
		if (MODELOBJECT_TARGET == this.storeTarget) {
			String v = encode(getValue().toString());
			String n = attribute.getName();
			if(modelObject.isActive()) {
				modelObject.getModel().editObjectAttribute(modelObject, n, v);
			} else {
				modelObject.setAttributeValue(attribute.getName(), getValue().toString());
			}
			String v1 = modelObject.getAttributeValue(n);
			if(!v1.equals(v)) {
				setValue(v1);
				fireValueChange(getValue().toString(), decode(v1));					
			}
		} else {
			attributeData.setValue(encode(getValue().toString()));
		}
	}

	public String getCommand() {
		return "...";
	}

	public String invoke(Control control) {
		String n = getAttribute().getName();
		String v = "" + getValue();
		Properties p = new Properties();
		p.put("shell", control.getShell());
		XModelObject o = modelObject.copy();
		o.setAttributeValue(n, v);
		XActionInvoker.invoke("EditActions.Edit_" + n, o, p);
		return o.getAttributeValue(n);
	}

}
