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

import org.eclipse.core.runtime.IConfigurationElement;

/**
 * @author Viacheslav Kabanovich
 */
public class Variable implements DecoratorConstants {
	public static Variable NAME = new Variable("name", "default label");

	String name;
	String description;
	
	public Variable() {}
	
	Variable(String name, String description) {
		this.name = name;
		this.description = description;
	}
	
	public String getName() {
		return name;
	}
	
	public String getDescription() {
		return description;
	}
	
	public String getRuleText() {
		return RULE_OPENING + name + RULE_CLOSING;
	}
	
	public String toString() {
		if(description == null || description.length() == 0) {
			return name;
		}
		return name + " - " + description;
	}
	
	public void load(IConfigurationElement element) {
		name = element.getAttribute(ATTR_NAME);
		description = element.getAttribute(ATTR_DESCRIPTION);
	}

}
