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

import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelMessages;

public class XAttributeConstraintNmtoken extends XAttributeConstraintProperties {

	char[][] intervals = {
		{':', ':'},	{'A', 'Z'},	{'_', '_'},	{'a', 'z'},	{0xC0, 0xD6},
		{0xD8, 0xF6},  {0xF8 , 0x2FF}, {0x370, 0x37D}, {0x37F, 0x1FFF}, 
		{0x200C, 0x200D}, {0x2070, 0x218F}, {0x2C00, 0x2FEF}, {0x3001, 0xD7FF}, 
		{0xF900, 0xFDCF}, {0xFDF0, 0xFFFD}, // {0x10000, 0xEFFFF}, 
		{'-', '-'}, {'.', '.'}, {'0', '9'},	
		{0xB7, 0xB7}, {0x0300, 0x036F}, {0x203F, 0x2040}, 
	};

    public XAttributeConstraintNmtoken() {}

    public boolean accepts(String value) {
        if(value == null) return false;
        if(value.length() == 0) return true;
        for (int i = 1; i < value.length(); i++) {
        	char ch = value.charAt(i);
        	boolean ok = false;
        	for (int k = 0; k < intervals.length; k++) {
        		if(ch >= intervals[k][0] && ch <= intervals[k][1]) {
        			ok = true;
        			break;
        		}
        	}
        	if(!ok) return false;
        }
        return true;
    }
   
    public String getError(String value) {
        return (value.length() == 0) ? (isRequired() ? ModelMessages.CONSTRAINT_NONEMPTY : null) :
               accepts(value) ? null :
               ModelMessages.CONSTRAINT_NMTOKEN;
    }

    boolean isRequired() {
    	return attribute != null && "always".equals(attribute.getProperty("save")); //$NON-NLS-1$ //$NON-NLS-2$
    }

    public String getCorrectedValue(String value) {
        if(value == null || value.length() == 0) return null;
        if(XModelObjectConstants.TRUE.equals(getProperties().getProperty("acceptIncorrect"))) return value; //$NON-NLS-1$
        //
        return value;
    }


}

