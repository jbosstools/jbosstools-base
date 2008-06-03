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
import org.jboss.tools.common.model.impl.*;

public class XMoveUndo extends XUndoableImpl {
    protected XModel model = null;
    protected String path = null;
    protected int from, to;
    protected String op;

    public XMoveUndo(XModelObject object, int from, int to) {
        model = object.getModel();
        path = object.getPath();
        this.from = from;
        this.to = to;
        op = object.getAttributeValue("element type") + " " +
             object.getModelEntity().getRenderer().getTitle(object);
        resetDescription();
    }

    private void resetDescription() {
        description = "" + (from + 1) + "-th child of " +
                      op + " moved to " + (to + 1) + "-th position";
    }

    public void doUndo() {
        execute(to, from);
    }

    public void doRedo() {
        execute(from, to);
    }

    protected void execute(int f, int t) {
        XModelObject object = model.getByPath(path);
        if(object == null || (!(object instanceof OrderedObjectImpl))) return;
        OrderedObjectImpl oo = (OrderedObjectImpl)object;
        oo.move(f, t, true);
        oo.setModified(true);
    }

    protected String getActionIcon() {
        return "images/actions/undo.gif";
    }

    protected boolean merge(XUndoableImpl u) {
        if(!(u instanceof XMoveUndo)) return false;
        XMoveUndo c = (XMoveUndo)u;
        if(path.equals(c.path) && to == c.from) {
            to = c.to;
            resetDescription();
            return true;
        }
        return false;
    }

}
