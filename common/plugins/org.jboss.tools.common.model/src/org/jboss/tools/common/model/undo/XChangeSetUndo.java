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

public class XChangeSetUndo extends XUndoableImpl {
    protected XModel model = null;
    protected String path = null;
    protected String[][] attr = null; // name, oldvalue, newvalue

    public XChangeSetUndo(XModelObject object, String[][] attr) {
        model = object.getModel();
        path = object.getPath();
        this.attr = attr;
        for (int i = 0; i < attr.length; i++)
          this.attr[i][2] = object.getAttributeValue(attr[i][0]);
        String[] types = object.getModelEntity().getRenderer().getIconNames();
        String iconType = types.length == 0 ? "main" : object.getModelEntity().getRenderer().getIconNames()[0];
        icon = new XModelObjectIcon(object).getEclipseImage0(new String[]{iconType});
        description = createDescription(object);
        kind = EDIT;
    }

    public void doUndo() {
        execute(1);
    }

    public void doRedo() {
        execute(2);
    }

    protected void execute(int k) {
        XModelObject object = model.getByPath(path);
        if(object == null) return;
        for (int i = 0; i < attr.length; i++) {
            object.setAttributeValue(attr[i][0], attr[i][k]);
        }
        object.setModified(true);
        path = object.getPath();
    }

    protected boolean merge(XUndoableImpl u) {
        if(!(u instanceof XChangeSetUndo)) return false;
        XChangeSetUndo c = (XChangeSetUndo)u;
        if(c == null || path == null || attr == null || !path.equals(c.path) || attr.length != c.attr.length) return false;
        if(attr.length != 1) return false;
        if(!attr[0][0].equals(c.attr[0][0])) return false;
        attr[0][2] = c.attr[0][2];
        return true;
    }

    protected String createDescription(XModelObject o) {
        return o.getAttributeValue("element type") + " " +
               o.getModelEntity().getRenderer().getTitle(o);
    }

}

