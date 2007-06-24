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
package org.jboss.tools.common.meta.impl;

import java.util.*;
import org.w3c.dom.*;
import org.jboss.tools.common.meta.*;

public abstract class XMetaElementImpl implements XMetaElement, XMetaDataConstants {
    protected String name;
    protected String displayName;
	protected Properties p = null;

    public XMetaElementImpl() {}

    public XModelMetaData getMetaModel() {
        return XModelMetaDataImpl.getInstance();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDisplayName() {
        return displayName == null ? getName() : displayName;
    }

    public void setDisplayName(String dname) {
        displayName = dname;
    }

    public abstract void load(Element el);

    public final String expand(String parameter, String mapping) {
        if(parameter == null) return null;
        if(parameter.startsWith("%")) {
            int j = parameter.length() - 1;
            try {
                parameter = getMetaModel().getMapping(mapping)
                                    .getValue(parameter.substring(1, j));
            } catch (Exception e) {
                return null;
            }
        }
        return parameter;
    }

	public String getProperty(String name) {
		return (p == null) ? null : p.getProperty(name);
	}
	
	protected void loadProperties(Element el) {
		p = parseProperties(el.getAttribute("PROPERTIES"));
	}

	protected static Properties parseProperties(String ps) {
		if(ps == null || ps.length() == 0) return null;
		Properties p = new Properties();
		StringTokenizer st = new StringTokenizer(ps, ";,");
		while(st.hasMoreElements()) {
			String t = st.nextToken();
			int i = t.indexOf('=');
			if(i < 0) continue;
			String n = t.substring(0, i).trim();
			String v = t.substring(i + 1).trim();
			p.setProperty(n, v);
		}
		return p;
	}

}
