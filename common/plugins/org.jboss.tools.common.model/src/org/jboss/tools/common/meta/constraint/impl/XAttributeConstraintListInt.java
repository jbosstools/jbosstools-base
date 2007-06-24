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

public class XAttributeConstraintListInt extends XAttributeConstraintAList {

    public XAttributeConstraintListInt() {}

    public boolean accepts(String value) {
        if(values.contains(value)) return true;
        try {
            return (Integer.parseInt(value) >= 0);
        } catch (Exception e) {
        	//ignore
            return false;
        }
    }

    public String getError(String value) {
        return accepts(value) ? null : getErrorById("CONSTRAINT_INTEGER_OR_LIST");
    }


}