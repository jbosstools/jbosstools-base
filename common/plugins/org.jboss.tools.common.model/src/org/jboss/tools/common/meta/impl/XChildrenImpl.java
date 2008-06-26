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

public class XChildrenImpl implements XMetaDataConstants {
    protected XChild[] children = new XChild[0];
    protected HashMap<String,XChild> children_map = new HashMap<String,XChild>(10);

    public XChildrenImpl() {}

    public XChild getChild(String entityName) {
        return children_map.get(entityName);
    }

    public XChild[] getChildren() {
        return children;
    }

    public void load(Element el) {
        Element p = XMetaDataLoader.getUniqueChild(el, XMODEL_CHILDREN);
        if(p == null) return;
        ArrayList<XChild> c = new ArrayList<XChild>();
        Element[] es = XMetaDataLoader.getChildrenElements(p, XMODEL_CHILD);
        for (int i = 0; i < es.length; i++) {
            XChildImpl m = new XChildImpl();
            m.load(es[i]);
            children_map.put(m.getName(), m);
            c.add(m);
        }
        children = c.toArray(new XChild[0]);
    }

    void validate(XModelMetaData meta) {
        String[] ns = children_map.keySet().toArray(new String[0]);
        ArrayList<XChild> c = new ArrayList<XChild>();
        for (int i = 0; i < children.length; i++) {
            String n = children[i].getName();
            if(meta.getEntity(n) == null) children_map.remove(n);
            else c.add(children_map.get(n));
        }
        if(children_map.size() < ns.length)
          children = c.toArray(new XChild[0]);
    }

    void merge(XChild[] ext) {
        ArrayList<XChild> l = new ArrayList<XChild>();
        for (int i = 0; i < ext.length; i++) {
            String n = ext[i].getName();
            if(children_map.get(n) != null) continue;
            children_map.put(n, ext[i]);
            l.add(ext[i]);
        }
        if(l.size() == 0) return;
        XChild[] nc = new XChild[children.length + l.size()];
        for (int i = 0; i < children.length; i++) nc[i] = children[i];
        for (int i = 0; i < l.size(); i++) nc[children.length + i] = l.get(i);
        children = nc;
    }

}
