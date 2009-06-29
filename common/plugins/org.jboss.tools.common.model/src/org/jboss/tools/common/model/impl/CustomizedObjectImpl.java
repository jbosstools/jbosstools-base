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
package org.jboss.tools.common.model.impl;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class CustomizedObjectImpl extends RegularObjectImpl {
	private static final long serialVersionUID = 1L;
	String attribute = null;
	
	public String getAttribute() {
		if(attribute != null) return attribute;
		XAttribute[] as = getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) {
			if(XModelObjectConstants.TRUE.equals(as[i].getProperty("id"))) { //$NON-NLS-1$
				attribute = as[i].getName();
				return attribute;
			}
		}
		ModelPlugin.getPluginLog().logInfo("No id attribute set for entity " + getModelEntity().getName()); //$NON-NLS-1$
		attribute = as[0].getName();
		return attribute;
	}
	
	public String name() {
		return "" + getAttributeValue(getAttribute()); //$NON-NLS-1$
	}

    protected RegularChildren createChildren() {
    	String children = getModelEntity().getProperty("children"); //$NON-NLS-1$
    	if(children == null || children.length() == 0) return new RegularChildren();
    	if(children.equals("%Ordered%")) return new OrderedChildren(); //$NON-NLS-1$
    	if(children.equals("%EntityOrdered%")) return new OrderedByEntityChildren(); //$NON-NLS-1$
    	//
        return new RegularChildren();
    }

}
