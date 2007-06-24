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

import org.jboss.tools.common.model.XModel;

public class XUndoManager {
    private XModel model = null;
    private XUndoableImpl start;
    private XUndoableImpl current;
    private int transaction = -1;
    private Object listener = null;

    public XUndoManager() {
        reset();
    }

    public void setModel(XModel model) {
        this.model = model;
    }

    public void reset() {
        start = new XUndoableT(null);
        current = start;
        fire();
    }

    public void undo() {
        if(!canUndo()) return;
        current.undo();
        current = current.prev();
    }

    public void redo() {
        if(!canRedo()) return;
        current = current.next();
        current.redo();
    }

    public boolean canUndo() {
        return (current != start && current.canUndo());
    }

    public boolean canRedo() {
        return (current.next() != null && current.next().canRedo());
    }

    public void addUndoable(XUndoable u) {
        XUndoableImpl w = (u instanceof XUndoableImpl) ? (XUndoableImpl)u :
                          new XUndoableT(u);
        if(current.merge(w)) return;
        current.setNext(w);
        current = w;
        if(transaction >= 0) ++transaction;
        reduce();
        fire();
    }

    private void reduce() {
        if(transaction >= 0) return;
        int capacity = 10;
        try {
          String c = model.getRoot("Preferences").getAttributeValue("undo capacity");
          capacity = Integer.parseInt(c);
        } catch (Exception e) {
        	//ignore
        }
        if(capacity == 0) {
            start.setNext(null);
            current = start;
            return;
        }
        int i = 0;
        XUndoableImpl c = current;
        while(true) {
            ++i;
            if(c == start) return;
            if(i >= capacity) {
                start.setNext(c);
                return;
            }
            c = c.prev();
        }
    }

    public XUndoList getList() {
        XUndoList list = new XUndoList(this);
        XUndoableImpl u = start;
        int s = -1;
        if(u == current) list.setCurrent(s);
        while(u.next() != null) {
            u = u.next();
            ++s;
            if(u == current) list.setCurrent(s);
            XUndoItem item = new XUndoItem(u.getDescription());
            item.setIcons(u.icons());
            list.add(item);
        }
        return list;
    }

    public void beginTransaction() {
        transaction = 0;
    }

    public int getTransactionStatus() {
        return transaction;
    }

    public void commitTransaction() {
        for (int i = 0; i < transaction; i++) current = current.prev();
        current.setNext(null);
        transaction = -1;
    }

    public void rollbackTransaction() {
        for (int i = 0; i < transaction; i++) undo();
        current.setNext(null);
        transaction = -1;
        fire();
    }

    public void rollbackTransactionInProgress() {
        if(current.next() != null) return;
        if(!(current instanceof XTransactionUndo)) return;
        XTransactionUndo t = (XTransactionUndo)current;
        if(!t.isInProgress()) return;
        try {
        	if(t.canUndo()) t.undo();
        } catch (Exception e) {
        	//ignore
        }
        current = t.prev();
        current.setNext(null);
        fire();
    }

    public void addListener(Object listener) {
        this.listener = listener;
    }

    private void fire() {
        if(listener == null) return;
        synchronized (listener) {
            listener.notify();
        }
    }

}

class XUndoableT extends XUndoableImpl {

    private XUndoable delegate = null;

    public XUndoableT(XUndoable delegate) {
        this.delegate = delegate;
    }

    public void doUndo() {}
    public void doRedo() {}
    public boolean canUndo() {
        return (delegate != null && delegate.canUndo());
    }
    public boolean canRedo() {
        return (delegate != null && delegate.canRedo());
    }
}

