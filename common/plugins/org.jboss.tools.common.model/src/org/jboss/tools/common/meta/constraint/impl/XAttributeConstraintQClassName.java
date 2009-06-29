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
import java.util.*;

public class XAttributeConstraintQClassName extends XAttributeConstraintImpl {
    private XAttributeConstraintJavaName x = new XAttributeConstraintJavaName();

    public XAttributeConstraintQClassName() {
    	x.allowPrimitiveTypes = true;
    }

    public boolean accepts(String value) {
        if(value == null) return false;
        if(value.length() == 0) return true;
        StringTokenizer st = new StringTokenizer(value, "."); //$NON-NLS-1$
        ////if(st.countTokens() < 2) return false;
        while(st.hasMoreTokens()) {
            String t = st.nextToken();
            if(!x.accepts(t)) return false;
        }
        return true;
    }

    public String getError(String value) {
        if(accepts(value)) return null;
        StringTokenizer st = new StringTokenizer(value, "."); //$NON-NLS-1$
        ////if(st.countTokens() < 2) return "must be a qualified name of class in a package";
        while(st.hasMoreTokens()) {
            String err = x.getError(st.nextToken());
            if(err != null) return MessageFormat.format("contains part that {0}", err);
        }
        return null;
    }

    public String getCorrectedValue(String value) {
        if(value == null) return ""; //$NON-NLS-1$
        StringBuffer sb = new StringBuffer();
        StringTokenizer st = new StringTokenizer(value, "."); //$NON-NLS-1$
        while(st.hasMoreTokens()) {
            String cor = x.getCorrectedValue(st.nextToken());
            if(cor == null) continue;
            if(sb.length() > 0) sb.append('.');
            sb.append(cor);
        }
        return (sb.length() == 0) ? null : sb.toString();
    }

}
