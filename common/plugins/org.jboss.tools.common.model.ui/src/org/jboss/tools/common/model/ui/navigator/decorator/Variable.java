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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author Viacheslav Kabanovich
 */
public class Variable implements DecoratorConstants {
	public static Variable NAME = new Variable("name", "default label");

	String name;
	String parameters = "";
	String description;
	ICustomVariable custom;
	
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
		return RULE_OPENING + name + parameters + RULE_CLOSING;
	}
	
	public String toString() {
		if(description == null || description.length() == 0) {
			return name;
		}
		return name + parameters + " - " + description;
	}
	
	public void load(IConfigurationElement element) {
		name = element.getAttribute(ATTR_NAME);
		int i = name.indexOf('(');
		if(i >= 0) {
			parameters = name.substring(i);
			name = name.substring(0, i);
		}
		description = element.getAttribute(ATTR_DESCRIPTION);
		String cls = element.getAttribute(ATTR_CLASS);
		if(cls != null && cls.length() > 0) {
			try {
				custom = (ICustomVariable)element.createExecutableExtension(ATTR_CLASS);
			} catch (CoreException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} catch (ClassCastException e1) {
				ModelUIPlugin.getPluginLog().logError("Attribute " + ATTR_CLASS + " must be instanceof ICustomVariable", e1);
			}
		}
	}

}
