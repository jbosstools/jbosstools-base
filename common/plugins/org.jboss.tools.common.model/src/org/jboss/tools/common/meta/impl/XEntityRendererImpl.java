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
import org.jboss.tools.common.model.*;

public class XEntityRendererImpl extends XMetaElementImpl implements XEntityRenderer{
    protected String[] m_Icons;
    protected Hashtable<String,XIconImpl> icons = new Hashtable<String,XIconImpl>();

    public XEntityRendererImpl() {}

    public String getProperty(String name) {
        return "";
    }
    
    public String[] getIconNames() {
        return m_Icons;
    }

    public String getIconInfo(String iconName) {
        XIconImpl ii = icons.get(iconName);
        return (ii == null) ? null : ii.getInfo();
    }

    public String getTitle(XModelObject object){
        return object.getPresentationString();
    }

    public void load(Element element) {
        loadProperties(element);
        loadIcons(element);
    }

    protected void loadProperties(Element element) {}

    private void loadIcons(Element element) {
        Element ic = XMetaDataLoader.getUniqueChild(element, ICONS);
        if(ic == null) return;
        Element[] cs = XMetaDataLoader.getChildrenElements(ic, ICON);
        ArrayList<String> v = new ArrayList<String>();
        for (int i = 0; i < cs.length; i++) {
            XIconImpl mi = new XIconImpl();
            mi.load(cs[i]);
            String tp = mi.getType();
            if(tp != null && tp.length() > 0) {
                icons.put(mi.getType(), mi);
                v.add(tp);
            }
        }
        m_Icons = v.toArray(new String[0]);
    }

}

class XIconImpl implements XMetaDataConstants {
    protected String type;
    protected String info;

    public XIconImpl() {}

    protected void load(Element element) {
        type = element.getAttribute(ICON_TYPE);
        info = element.getAttribute(ICON_INFO);
    }

    public String getType() {
        return type;
    }

    public String getInfo() {
        return info;
    }

}

