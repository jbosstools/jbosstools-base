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

import java.util.Properties;

import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.IPropertySourceProvider;

import org.jboss.tools.common.model.XModelObject;

public class PropertySourceProvider implements IPropertySourceProvider {

	public PropertySourceProvider() {
	}
	
	private static PropertySourceProvider propertySourceProvider = null;
	
	public static PropertySourceProvider getInstance() {
		if (propertySourceProvider==null) {
			propertySourceProvider = new PropertySourceProvider();
		}
		return  propertySourceProvider;
	}
	
	public IPropertySource getPropertySource(Object object) {
		IPropertySource propertySource = null;
		if (object instanceof XModelObject) {
			XModelObjectPropertySource s = new XModelObjectPropertySource();
			s.setModelObject((XModelObject)object);
			propertySource = s;
		}
		if (object instanceof Properties) {
			propertySource = new WizardDataPropertySource((Properties)object);
		}
		return propertySource;
	}
}
