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
package org.jboss.tools.common.meta.constraint.impl;

import org.w3c.dom.*;

public class XAttributeConstraintInt extends XAttributeConstraintProperties {
    protected boolean mandatory = true;
    protected int min = Integer.MIN_VALUE;
    protected int max = Integer.MAX_VALUE;
    public XAttributeConstraintInt() {
        super();
    }

    public void load(Element element) {
        super.load(element);
        mandatory = getBoolean("mandatory", true);
        min = getInt("minimum", Integer.MIN_VALUE);
        max = getInt("maximum", Integer.MAX_VALUE);
    }

    public boolean accepts(String value) {
        if(value == null || value.length() == 0) return !mandatory;
        try {
            int i = Integer.parseInt(value);
            return (i >= min && i <= max);
        } catch (Exception e) {
        	//ignore
            return false;
        }
    }

    public String getError(String value) {
    	if(accepts(value)) return null;
    	if(min == Integer.MIN_VALUE && max == Integer.MAX_VALUE) {
    		return "must be an integer.";
    	} else if(min == 0 && max == Integer.MAX_VALUE) {
    		return "must be a non-negative integer.";
    	} else if(min > Integer.MIN_VALUE && max == Integer.MAX_VALUE) {
    		return "must be an integer greater than " + min + ".";
    	} else if(min == Integer.MIN_VALUE && max < Integer.MAX_VALUE) {
    		return "must be an integer less than " + max + ".";
    	} else {
    		return "must be an integer from " + min + " to " + max + ".";
    	}
    }

}
