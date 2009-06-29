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

import java.text.MessageFormat;
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
        mandatory = getBoolean("mandatory", true); //$NON-NLS-1$
        min = getInt("minimum", Integer.MIN_VALUE); //$NON-NLS-1$
        max = getInt("maximum", Integer.MAX_VALUE); //$NON-NLS-1$
    }

    public boolean accepts(String value) {
        if(value == null || value.length() == 0) return !mandatory;
        try {
            int i = Integer.parseInt(value);
            return (i >= min && i <= max);
        } catch (NumberFormatException e) {
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
    		return MessageFormat.format("must be an integer greater than {0}.",
					min);
    	} else if(min == Integer.MIN_VALUE && max < Integer.MAX_VALUE) {
    		return MessageFormat.format("must be an integer less than {0}.", max);
    	} else {
    		return MessageFormat.format("must be an integer from {0} to {1}.",
					min, max);
    	}
    }

}
