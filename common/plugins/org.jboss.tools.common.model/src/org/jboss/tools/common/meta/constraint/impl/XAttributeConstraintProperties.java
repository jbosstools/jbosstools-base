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

public class XAttributeConstraintProperties extends XAttributeConstraintImpl {
    protected Properties p = new Properties();

    public XAttributeConstraintProperties() {}

    public void load(Element element) {
        super.load(element);
        NodeList nl = element.getElementsByTagName(VALUE);
        for (int i = 0; i < nl.getLength(); i++) {
            Element c = (Element)nl.item(i);
            String vs = c.getAttribute(NAME);
            int n = vs.indexOf('=');
            String k = (n < 0) ? "" : vs.substring(0, n).trim();
            String v = vs.substring(n + 1).trim();
            p.setProperty(k, v);
        }
    }

    public Properties getProperties() {
        return p;
    }

    public int getInt(String name, int def) {
        String v = p.getProperty(name);
        if(v == null || v.length() == 0) return def;
        try {
            return Integer.parseInt(v);
        } catch (Exception e) {
        	//ignore
            return def;
        }
    }

    public boolean getBoolean(String name, boolean def) {
        String v = p.getProperty(name);
        if(v == null || v.length() == 0) return def;
        try {
            return Boolean.getBoolean(v);
        } catch (Exception e) {
        	//ignore
            return def;
        }
    }

}

