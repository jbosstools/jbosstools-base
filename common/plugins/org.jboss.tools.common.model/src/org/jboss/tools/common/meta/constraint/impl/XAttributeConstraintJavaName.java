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

import java.util.*;

import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.w3c.dom.*;

public class XAttributeConstraintJavaName extends XAttributeConstraintProperties {

    private static final String[] keywords = {"break", "case", //$NON-NLS-1$ //$NON-NLS-2$
        "catch", "class", "const", "continue", "default", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        "else", "extends", XModelObjectConstants.FALSE, "final", "finally", "for", "if", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        "implements", "import", "instanceof", "interface", "new", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        "null", "package", "protected", "private", "public", "return", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        "static", "switch", "synchronized", "throw", "throws", "transient", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        XModelObjectConstants.TRUE, "try", "void", "volatile", "while"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    private static final String[] primitives = {
    	"boolean", "byte", "char", "double", "float", "int", "long", "short" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    };

    private static final Set<String> keytable = new HashSet<String>();
    private static final Set<String> primitiveSet = new HashSet<String>();
    static {
        for (int i = 0; i < keywords.length; i++) keytable.add(keywords[i]);
        for (int i = 0; i < primitives.length; i++) primitiveSet.add(primitives[i]);
    }

    private Set<String> custom = null;
    
    boolean allowPrimitiveTypes = false;

    public void load(Element element) {
        super.load(element);
        String k = p.getProperty("keywords"); //$NON-NLS-1$
        if(k == null) return;
        StringTokenizer st = new StringTokenizer(k, ";,"); //$NON-NLS-1$
        custom = new HashSet<String>();
        while(st.hasMoreTokens()) custom.add(st.nextToken());
    }

    public XAttributeConstraintJavaName() {}

    public boolean accepts(String value) {
        if(value == null) return false;
        if(value.length() == 0) return true;
        if(!Character.isJavaIdentifierStart(value.charAt(0))) return false;
        if(keytable.contains(value) && !XModelObjectConstants.TRUE.equals(p.getProperty("acceptKeyWord"))) return false; //$NON-NLS-1$
        if(!allowPrimitiveTypes && primitiveSet.contains(value)) return false;
        if(custom != null && custom.contains(value)) return false;
        for (int i = 1; i < value.length(); i++) {
            if(!Character.isJavaIdentifierPart(value.charAt(i))) return false;
        }
        return true;
    }

    public String getError(String value) {
        return (value.length() == 0) ? ModelMessages.CONSTRAINT_NONEMPTY :
               accepts(value) ? null :
               (keytable.contains(value)) ? ModelMessages.CONSTRAINT_NO_JAVA_KEYWORD :
               (!allowPrimitiveTypes && primitiveSet.contains(value)) ? ModelMessages.CONSTRAINT_NO_JAVA_KEYWORD :
               (custom != null && custom.contains(value)) ? ModelMessages.IS_A_RESERVED_WORD :
                ModelMessages.CONSTRAINT_JAVA_NAME;
    }

    public String getCorrectedValue(String value) {
        if(value == null || value.length() == 0) return null;
        if(XModelObjectConstants.TRUE.equals(getProperties().getProperty("acceptIncorrect"))) return value; //$NON-NLS-1$
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if(first) {
                if(!Character.isJavaIdentifierStart(c)) continue;
                first = false;
            } else if(!Character.isJavaIdentifierPart(c)) continue;
            sb.append(c);
        }
        return (sb.length() == 0) ? null : sb.toString();
    }


}

