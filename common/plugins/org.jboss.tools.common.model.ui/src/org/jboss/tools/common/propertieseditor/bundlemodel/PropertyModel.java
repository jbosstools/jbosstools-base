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
package org.jboss.tools.common.propertieseditor.bundlemodel;

import java.util.*;

import org.jboss.tools.common.model.XModelObject;

public class PropertyModel {
	Map<String,String> values = new HashMap<String,String>();
	XModelObject object;
	BundleModel bundleModel;
	
	public PropertyModel(BundleModel bundleModel) {
		this.bundleModel = bundleModel;
	}
	
	public void setModelObject(XModelObject object) {
		this.object = object;
		object.setObject("propertyModel", this);
	}
	
	public XModelObject getModelObject() {
		return object;
	}
	
	public String getName() {
		return object.getAttributeValue("name");
	}
	
	public boolean hasValue(String locale) {
		return values.get(locale) != null;
	}
	
	public String getValue(String locale) {
		Object v = values.get(locale);
		if(v == null) v = values.get("");
		return (v == null) ? "" : v.toString();
	}
	
	public void setValue(String locale, String value) {
		if(value == null) {
			if(locale.length() == 0) values.clear();
			else values.remove(locale);
		} else {
			values.put(locale, value);
		}
	}
	
	public void setLocale(String locale) {
		object.setAttributeValue("value", getValue(locale));
	}
	
	public void commit() {
		String locale = bundleModel.getCurrentLocale();
		String v = getValue(locale);
		setValue(locale, object.getAttributeValue("value"));
		if(!v.equals(getValue(locale))) bundleModel.setModified(true);		
	}

}
