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

public class XAttributeConstraintXMLName extends XAttributeConstraintImpl {

    public XAttributeConstraintXMLName() {}

    public boolean accepts(String value) {
        if(value == null) return false;
        if(value.length() == 0) return true;
        if(!isXMLStartChar(value.charAt(0))) return false;
        for (int i = 1; i < value.length(); i++) {
            if(!isXMLPartChar(value.charAt(i))) return false;
        }
        return true;
    }

    public String getError(String value) {
        return accepts(value) ? null : getErrorById("CONSTRAINT_XML_NAME");
    }

    public static final boolean isXMLStartChar(char c) {
        return Character.isLetter(c) || (c == '_') || (c == ':');
    }

    public static final boolean isXMLPartChar(char c) {
        return isXMLStartChar(c) || (c == '.') || (c == '-') || Character.isDigit(c);
    }
}
