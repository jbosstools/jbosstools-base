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

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.util.IconUtil;

public abstract class XUndoableImpl implements XUndoable {
    public static final int EDIT = 0;
    public static final int ADD = 1;
    public static final int REMOVE = 2;
    protected int kind;
    protected XUndoableImpl next = null;
    protected XUndoableImpl prev = null;
    protected boolean undone = false;

    protected String description = "";
    protected Image icon = null;

    public XUndoableImpl() {}

    public void setNext(XUndoableImpl next) {
        this.next = next;
        if(next != null) next.prev = this;
    }

    public XUndoableImpl prev() {
        return prev;
    }

    public XUndoableImpl next() {
        return next;
    }

    public void undo() {
        doUndo();
        undone = true;
    }

    public void redo() {
        doRedo();
        undone = false;
    }

    public boolean canUndo() {
        return !undone;
    }

    public boolean canRedo() {
        return undone;
    }

    protected abstract void doUndo();
    protected abstract void doRedo();

    public String getDescription() {
        return description;
    }

    public Image[] icons() {
        return new Image[] {IconUtil.getEclipseImage(getActionIcon()), icon};
    }

    protected String getActionIcon() {
        return (kind == ADD) ? "images/java/newattribute.gif" :
               (kind == REMOVE) ? "images/actions/delete.gif" :
                                  "images/file/redhat_file.gif";
    }

    protected boolean merge(XUndoableImpl u) {
        return false;
    }

}

