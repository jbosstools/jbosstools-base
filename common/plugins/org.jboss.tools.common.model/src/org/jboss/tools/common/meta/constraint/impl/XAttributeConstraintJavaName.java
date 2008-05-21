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
import org.w3c.dom.*;

public class XAttributeConstraintJavaName extends XAttributeConstraintProperties {

    private static final String[] keywords = {"break", "case",
        "catch", "class", "const", "continue", "default",
        "else", "extends", "false", "final", "finally", "for", "if",
        "implements", "import", "instanceof", "interface", "new",
        "null", "package", "protected", "private", "public", "return",
        "static", "switch", "synchronized", "throw", "throws", "transient",
        "true", "try", "void", "volatile", "while"};
    private static final String[] primitives = {
    	"boolean", "byte", "char", "double", "float", "int", "long", "short"
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
        String k = p.getProperty("keywords");
        if(k == null) return;
        StringTokenizer st = new StringTokenizer(k, ";,");
        custom = new HashSet<String>();
        while(st.hasMoreTokens()) custom.add(st.nextToken());
    }

    public XAttributeConstraintJavaName() {}

    public boolean accepts(String value) {
        if(value == null) return false;
        if(value.length() == 0) return true;
        if(!Character.isJavaIdentifierStart(value.charAt(0))) return false;
        if(keytable.contains(value) && !"true".equals(p.getProperty("acceptKeyWord"))) return false;
        if(!allowPrimitiveTypes && primitiveSet.contains(value)) return false;
        if(custom != null && custom.contains(value)) return false;
        for (int i = 1; i < value.length(); i++) {
            if(!Character.isJavaIdentifierPart(value.charAt(i))) return false;
        }
        return true;
    }

    public String getError(String value) {
        return (value.length() == 0) ? getErrorById("CONSTRAINT_NONEMPTY") :
               accepts(value) ? null :
               (keytable.contains(value)) ? getErrorById("CONSTRAINT_NO_JAVA_KEYWORD") :
               (!allowPrimitiveTypes && primitiveSet.contains(value)) ? getErrorById("CONSTRAINT_NO_JAVA_KEYWORD") :
               (custom != null && custom.contains(value)) ? "is a reserved word" :
               getErrorById("CONSTRAINT_JAVA_NAME");
    }

    public String getCorrectedValue(String value) {
        if(value == null || value.length() == 0) return null;
        if("true".equals(getProperties().getProperty("acceptIncorrect"))) return value;
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

