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

public class XAttributeConstraintFolderName extends XAttributeConstraintImpl {
    private static String forbid = "\\/:*\""; //$NON-NLS-1$

    public XAttributeConstraintFolderName() {}

    public boolean accepts(String value) {
        if(value == null) return true;
        int l = value.length();
        for (int i = 0; i < l; i++)
          if(forbid.indexOf(value.charAt(i)) >= 0) return false;
        if(value.startsWith(".")) { //$NON-NLS-1$
            return !value.equals(".") && !value.equals(".."); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return true;
    }

    public String getError(String value) {
        if(accepts(value)) return null;
        int l = value.length();
        for (int i = 0; i < l; i++) {
            char c = value.charAt(i);
            if(forbid.indexOf(c) >= 0) return MessageFormat.format("contains illegal symbol {0}", c);
        }
        if(value.equals(".") || value.equals("..")) { //$NON-NLS-1$ //$NON-NLS-2$
            return "cannot be equal to a path reserved for navigation";
        }
        return null;
    }

}
