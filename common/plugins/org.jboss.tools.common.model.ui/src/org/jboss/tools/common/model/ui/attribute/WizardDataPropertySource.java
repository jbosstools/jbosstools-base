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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;

/**
 * @author Aleksei
 *
 */
public class WizardDataPropertySource implements IPropertySource, IXModelSupport {

	private XAction action;
	private XEntityData data; 
	private XModelObject object;
	private XAttributeData[] attributeData;
	private HashMap<Object,XAttributeData> hashMap;
	private IPropertyDescriptor[] propertyDescriptors;

	protected WizardDataPropertySource() {
	}

	public WizardDataPropertySource(Properties properties) {
		if(properties==null)return;
//		Enumeration keys = properties.propertyNames();
//		while (keys.hasMoreElements()) {
//			String key = keys.nextElement().toString();
//			String value = properties.get(key).toString();
//		} 
		
		action = (XAction)properties.get("action");
		object = (XModelObject)properties.get("object");
		data = action.getEntityData(this.object)[0];
		attributeData = data.getAttributeData();

		// create propertyDescriptors;
		ArrayList<IPropertyDescriptor> list = new ArrayList<IPropertyDescriptor>();
		hashMap = new HashMap<Object,XAttributeData>();
		XAttributePropertyDescription temp;
		for (int i=0;i<attributeData.length;++i) {
			temp = new XAttributePropertyDescription(this, attributeData[i].getAttribute(), attributeData[i], object.getModel()); 
			list.add(temp);
			hashMap.put(temp.getId(),attributeData[i]);
		}
		propertyDescriptors = list.toArray(new XAttributePropertyDescription[list.size()]);
	}

	public Object getEditableValue() {
		return null;
	}

	public IPropertyDescriptor[] getPropertyDescriptors() {
		return propertyDescriptors;
	}

	public Object getPropertyValue(Object id) {
		XAttributeData attributeData = (XAttributeData)hashMap.get(id);
		return attributeData.getValue();
	}

	public boolean isPropertySet(Object id) {
		return false;
	}

	public void resetPropertyValue(Object id) {
	}

	public void setPropertyValue(Object id, Object value) {
		XAttributeData attributeData = (XAttributeData)hashMap.get(id);
		attributeData.setValue((value == null) ? null : value.toString());
	}

	// IXModelSupport
	public XModel getModel() {
		return object.getModel();
	}

}
