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

import org.jboss.tools.common.model.XModelObject;

/**
 * @author Viacheslav Kabanovich
 */
public class AttributeDecoratorPart implements IDecoratorPart {
	Variable variable;
	String parameters = ""; //$NON-NLS-1$
	
	public AttributeDecoratorPart(Variable variable) {
		this.variable = variable;
	}
	
	public void setParameters(String params) {
		parameters = params;
	}

	public String getLabelPart(XModelObject object) {
		if(variable.custom != null) {
			return variable.custom.getLabelPart(object, parameters);
		}
		String v = object.getAttributeValue(variable.getName());
		return v == null ? "{" + variable.getName() + "}" : v;  //$NON-NLS-1$ //$NON-NLS-2$
	}

}
