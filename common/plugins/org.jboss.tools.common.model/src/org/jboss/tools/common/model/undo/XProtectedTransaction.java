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
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.impl.XModelImpl;

public class XProtectedTransaction {
    public interface Executor {
        public void execute() throws Exception;
    }

    public XProtectedTransaction() {}

    public void execute(XTransactionUndo u, Executor exec, XModelObject listener) throws Exception {
        XUndoManager undo = listener.getModel().getUndoManager();
        undo.addUndoable(u);
        try {
            BeginTransactionUndo ub = new BeginTransactionUndo(listener);
            ub.fire();
            undo.addUndoable(ub);
            exec.execute();
            EndTransactionUndo ue = new EndTransactionUndo(listener);
            ue.fire();
            undo.addUndoable(ue);
        } catch (Exception e) {
            undo.rollbackTransactionInProgress();
            throw e;
        } finally {
            u.commit();
        }
    }
}

class BeginTransactionUndo extends XUndoableImpl {
    protected XModelImpl mi = null;
    protected XModelObject object = null;

    public BeginTransactionUndo(XModelObject o) {
        object = o;
        mi = (XModelImpl)o.getModel();
    }

    protected void doUndo() {
        mi.fireStructureChanged(object, XModelTreeEvent.STRUCTURE_CHANGED, "transaction_end");
    }

    protected void doRedo() {
        mi.fireStructureChanged(object, XModelTreeEvent.STRUCTURE_CHANGED, "transaction_begin");
    }

    public void fire() {
        doRedo();
    }
}

class EndTransactionUndo extends BeginTransactionUndo {
    public EndTransactionUndo(XModelObject o) {
        super(o);
    }

    protected void doUndo() {
        super.doRedo();
    }

    protected void doRedo() {
        super.doUndo();
    }

}

