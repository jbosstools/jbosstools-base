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

import org.w3c.dom.*;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.meta.action.XActionItem;

public class XActionItemImpl extends XMetaElementImpl implements XActionItem {
    private XActionItemImpl parent = null;
    protected String iconname = null;

    public XActionItemImpl() {}

    public void setParent(XActionItemImpl parent) {
        this.parent = parent;
    }

    public String getPath() {
        if(parent == null) return null;
        String pp = parent.getPath();
        return (pp == null) ? getName() : pp + "/" + getName();
    }

	public String getIconKey() {
		return iconname;
	}
    public Image getImage() {
        return getMetaModel().getIconList().getImage(iconname);
    }

    public XActionItem getItem(String name) {
        return null;
    }

    public void load(Element el) {
        setName(el.getAttribute(NAME));
        setDisplayName(el.getAttribute(DISPLAYNAME));
        iconname = el.getAttribute(ICON);
		loadProperties(el);
    }

    public XActionItem copy(XActionItem.Acceptor acceptor) {
        XActionItemImpl item = createInstance();
        item.setName(getName());
        item.setDisplayName(getDisplayName());
        item.iconname = iconname;
        item.p = p;
        return item;
    }

    protected XActionItemImpl createInstance() {
        return null;
    }

}
