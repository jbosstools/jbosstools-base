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
package org.jboss.tools.common.meta.action.impl;

import java.util.*;
import org.w3c.dom.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.impl.*;

public class XActionListImpl extends XActionItemImpl implements XActionList {
    private XActionItem[] items;
    private short groupfactor = 0;
    private Element element = null;

    public XActionListImpl() {}

    public void setElement(Element element) {
        this.element = element;
        if(element != null) super.load(element);
    }

    public XActionItem getByPath(String path) {
        int i = (path == null) ? -1 : path.indexOf("/");
        if(i < 0) return getItem(path);
        XActionItem ci = getItem(path.substring(0, i));
        return (!(ci instanceof XActionListImpl)) ? null :
               ((XActionListImpl)ci).getByPath(path.substring(i + 1));
    }

    public boolean isLoaded() {
        return (element == null);
    }

    public void validate() {
        if(isLoaded()) return;
        synchronized(this) {
            if(element == null) return;
            load(element);
            element = null;
        }
    }

    public void print(String off) {
        XActionItem[] is = (XActionItem[])getActionItems();
        for (int i = 0; i < is.length; i++) {
            XActionItemImpl item = (XActionItemImpl)is[i];
            if(item instanceof XActionListImpl)
              ((XActionListImpl)item).print(off + "  ");
        }
    }

    public XActionItem[] getActionItems() {
        return items;
    }
    public short getGroupFactor() {
        return groupfactor;
    }

    public XActionItem getItem(String name) {
        for (int i = 0; i < items.length; i++) {
            if(items[i].getName().equals(name)) return items[i];
        }
        return null;
    }

    public XAction getAction(String path) {
        if(path == null) return null;
        int d = path.indexOf('.');
        if(d < 0) {
            XActionItem i = getItem(path);
            return (i instanceof XAction) ? (XAction)i : null;
        }
        String name = path.substring(0, d);
        path = path.substring(d + 1);
        XActionItem i = getItem(name);
        return (i instanceof XActionList) ? ((XActionList)i).getAction(path) : null;
    }

    public void load(Element el) {
        super.load(el);
        groupfactor = (short)XMetaDataLoader.getInt(el, "group", 0);
        Element[] cs = XMetaDataLoader.getChildrenElements(el, "XActionItem");
        items = new XActionItem[cs.length];
        for (int i = 0; i < items.length; i++) {
            Element ei = cs[i];
            String kind = ei.getAttribute("kind");
            XActionItemImpl item = (kind.equals("list"))
                 ? (XActionItemImpl) new XActionListImpl()
                 : (XActionItemImpl) new XActionImpl();
            item.setParent(this);
            items[i] = item;
            item.load(ei);
        }
    }

    public void merge(XActionListImpl ext) {
        validate();
        XActionItem[] ext_items = ext.getActionItems();
        Set<String> set = new HashSet<String>();
        ArrayList<XActionItem> l = new ArrayList<XActionItem>();
        for (int i = 0; i < items.length; i++) set.add(items[i].getName());
        for (int i = 0; i < ext_items.length; i++) {
            String n = ext_items[i].getName();
            if(!set.contains(n)) {
                ((XActionItemImpl)ext_items[i]).setParent(this);
                l.add(ext_items[i]);
            } else {
                XActionItem item = getItem(n);
                if((item instanceof XActionListImpl) && (ext_items[i] instanceof XActionListImpl)) {
                    ((XActionListImpl)item).merge((XActionListImpl)ext_items[i]);
                }
            }
        }
        if(l.size() == 0) return;
        XActionItem[] nc = new XActionItem[items.length + l.size()];
        for (int i = 0; i < items.length; i++) nc[i] = items[i];
        for (int i = 0; i < l.size(); i++) nc[items.length + i] = l.get(i);
        items = nc;
    }

    public XActionItem copy(XActionItem.Acceptor acceptor) {
        if(!acceptor.accepts(this)) return null;
        XActionListImpl item = (XActionListImpl)super.copy(acceptor);
        item.groupfactor = groupfactor;
        ArrayList<XActionItem> l = new ArrayList<XActionItem>();
        for (int i = 0; i < items.length; i++) {
             XActionItem ic = items[i].copy(acceptor);
             if(ic != null) l.add(ic);
        }
        item.items = l.toArray(new XActionItem[l.size()]);
        return item;
    }

    protected XActionItemImpl createInstance() {
        return new XActionListImpl();
    }

    public void addActionItem(XActionItem item) {
        for (int i = 0; i < items.length; i++)
          if(items[i].getName().equals(item.getName())) return;
        XActionItem[] is = new XActionItem[items.length + 1];
        System.arraycopy(items, 0, is, 0, items.length);
        is[items.length] = item;
        items = is;
    }

}

