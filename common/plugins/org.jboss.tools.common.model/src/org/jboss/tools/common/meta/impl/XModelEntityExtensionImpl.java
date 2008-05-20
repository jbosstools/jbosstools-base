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
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;

public class XModelEntityExtensionImpl extends XMetaElementImpl {
    protected XChildrenImpl children = new XChildrenImpl();
    protected XActionListImpl actions = new XActionListImpl();
    protected Element element = null;

    public XModelEntityExtensionImpl() {}

    void setElement(Element element) {
        this.element = element;
        setName(element.getAttribute(NAME));
    }

    void validate() {
        if(element == null) return;
        synchronized(this) {
            load(element);
            element = null;
        }
    }

    public XActionList getActionList(){
        actions.validate();
        return actions;
    }

    public XChild[] getChildren() {
        return children.getChildren();
    }

    public void load(Element element) {
        setName(element.getAttribute(NAME));
        children.load(element);
        Element ei = XMetaDataLoader.getUniqueChild(element, "XActionItem");
        actions.setElement(ei);
    }

}
