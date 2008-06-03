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
package org.jboss.tools.common.model.icons.impl;

import java.util.*;

import org.eclipse.swt.graphics.Image;
import org.w3c.dom.*;
import org.jboss.tools.common.model.icons.XIconList;
import org.jboss.tools.common.meta.impl.*;
import org.jboss.tools.common.model.util.IconUtil;

public class XIconListImpl extends XMetaElementImpl implements XIconList, XMetaDataConstants {
    private Hashtable<String,String> iconnames = new Hashtable<String,String>();
    private Hashtable<String,Image> eclipseImages = new Hashtable<String,Image>();

    public XIconListImpl() {}

    public String getIconPath(String name, String defaultName) {
        String s = (String)iconnames.get(name);
        return (s == null) ? (String)iconnames.get(defaultName) : s;
    }

	public Image getImage(String name, String defaultname) {
		Image i = getImage(name);
		if(i == null) {
			i = getImage(defaultname);
			if(i != null) eclipseImages.put(name, i);
		}
		return i;
	}

	public Image getImage(String name) {
		Image image = (Image)eclipseImages.get(name);
		if(image == null) {
			String picture = (String)iconnames.get(name);
			if(picture == null) return null;
			image = IconUtil.getEclipseImage(picture);
			eclipseImages.put(name, image);
		}
		return image;		
	}

    public void load(Element element) {
        load("", element);
    }

    protected void load(String prefix, Element element) {
        if(element == null) return;
        NodeList nl = element.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);
            if(n.getNodeType() != Node.ELEMENT_NODE) continue;
            Element c = (Element)n;
            String dp = c.getNodeName();
            if(dp.equals(ICON)) {
                String nm = prefix + c.getAttribute(NAME);
                String pt = c.getAttribute(ICON_PATH);
                iconnames.put(nm, pt);
            } else if(dp.equals("GROUP")) {
                String nm = c.getAttribute(NAME);
                load(prefix + nm + ".", c);
            } else {
                load(prefix + dp + ".", c);
            }
        }
    }

}
