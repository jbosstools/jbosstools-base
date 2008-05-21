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

import java.util.*;

public class XTransactionUndo extends XUndoableImpl {
    protected boolean progress = true;
    protected ArrayList<XUndoable> components = new ArrayList<XUndoable>();

    public XTransactionUndo(String description, int kind) {
       this.description = description;
       this.kind = (kind < 0 || kind > 2) ? ADD : kind;
    }

    protected void doUndo() {
        commit();
        for (int i = components.size() - 1; i >= 0; i--) {
            XUndoable u = components.get(i);
            if(u.canUndo()) u.undo();
        }
    }

    protected void doRedo() {
        commit();
        for (int i = 0; i < components.size(); i++) {
            XUndoable u = components.get(i);
            if(u.canRedo()) u.redo();
        }
    }

    protected boolean merge(XUndoableImpl u) {
        if(!isInProgress()) return false;
        components.add(u);
        return true;
    }

    public void commit() {
        progress = false;
    }

    boolean isInProgress() {
        return progress;
    }

}
