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
package org.jboss.tools.common.model.undo;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.icons.impl.XModelObjectIcon;

public class XCreateUndo extends XUndoableImpl {
    private XModel model = null;
    private String parentpath = null;
    private String childpath = null;
    private XModelObject child = null;

    public XCreateUndo(XModelObject parent, XModelObject child) {
        this.model = parent.getModel();
        parentpath = parent.getPath();
        childpath = child.getPathPart();
        description = child.getAttributeValue("element type") + " " +
                      child.getModelEntity().getRenderer().getTitle(child) + " in " +
                      parent.getAttributeValue("element type") + " " +
                      parent.getModelEntity().getRenderer().getTitle(parent);
        String[] types = child.getModelEntity().getRenderer().getIconNames();
        String iconType = types.length == 0 ? "main" : child.getModelEntity().getRenderer().getIconNames()[0];
        icon = new XModelObjectIcon(child).getEclipseImage0(new String[]{iconType});
        kind = ADD;
    }

    public void doUndo() {
        XModelObject parent = model.getByPath(parentpath);
        if(parent == null) return;
        XModelObject c = parent.getChildByPath(childpath);
        if(c != null) {
            parent.removeChild(c);
            parent.setModified(true);
            child = c.copy(false);
        }
    }

    public void doRedo() {
        XModelObject parent = model.getByPath(parentpath);
        if(child != null) {
            parent.addChild(child);
            child.setModified(true);
        }
    }

    protected String getActionIcon() {
        return "images/java/newattribute.gif";
    }

}

