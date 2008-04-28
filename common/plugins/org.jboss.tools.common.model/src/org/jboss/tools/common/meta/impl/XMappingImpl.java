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

public class XMappingImpl extends XMetaElementImpl implements XMapping {
    protected Properties map = new Properties();
    private String[] keys = null;

    public XMappingImpl() {}

    public String[] getKeys() {
        return keys;
    }

    public String getValue(String key) {
        String s = map.getProperty(key);
        return (s != null && s.length() > 0) ? s : map.getProperty(DEFAULT_KEY);
    }

    public void load(Element el) {
        setName(el.getAttribute(NAME));
        Element[] es = XMetaDataLoader.getChildrenElements(el, PAIR);
        for (int i = 0; i < es.length; i++) {
            String n = es[i].getAttribute(NAME);
            String v = es[i].getAttribute(VALUE);
            map.put(n, v);
        }
        keys = (String[])map.keySet().toArray(new String[0]);
    }

}
