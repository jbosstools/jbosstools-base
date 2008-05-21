/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.navigator.decorator;

import java.util.Properties;

import org.eclipse.core.runtime.IConfigurationElement;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;

/**
 * @author Viacheslav Kabanovich
 */
public class Example implements DecoratorConstants {
	
	public static XModelObject load(IConfigurationElement element) {
		String entity = element.getAttribute(ATTR_ENTITY);
		if(entity == null || entity.length() == 0) {
			return null;
		}
		Properties p = new Properties();
		IConfigurationElement[] cs = element.getChildren(NODE_PUT);
		for (int i = 0; i < cs.length; i++) {
			String name = cs[i].getAttribute(ATTR_NAME);
			String value = cs[i].getAttribute(ATTR_VALUE);
			if(name == null || value == null) continue;
			p.setProperty(name, value);
		}
		
		return PreferenceModelUtilities.getPreferenceModel().createModelObject(entity, p);
	}

}
