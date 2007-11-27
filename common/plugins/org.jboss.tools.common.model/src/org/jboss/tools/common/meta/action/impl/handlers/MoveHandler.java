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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.undo.*;
import org.jboss.tools.common.meta.action.impl.*;

public class MoveHandler extends AbstractHandler {

    public MoveHandler() {
        data = new XEntityDataImpl[0];
    }

    public boolean isEnabled(XModelObject object) {
        XModelObject o = object.getModel().getModelBuffer().source();
        if(o == null || object == null) return false;
        XModelObject p = object.getParent();
        return (p != null && o.getParent() == p &&
               p.isObjectEditable() && (p instanceof XOrderedObject) && ((XOrderedObject)p).areChildrenOrdered());
    }

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object)) return;
        XModelObject o = object.getModel().getModelBuffer().source();
        XModelObject p = object.getParent();
        XOrderedObject oo = (XOrderedObject)p;
        int to = oo.getIndexOfChild(object);
        int from = oo.getIndexOfChild(o);
        boolean r = oo.move(from, to, true);
        if(!r) return;
        if(object.isActive()) {
			XUndoManager undo = object.getModel().getUndoManager();
			undo.addUndoable(new XMoveUndo(p, from, to));
        }
    }
}
