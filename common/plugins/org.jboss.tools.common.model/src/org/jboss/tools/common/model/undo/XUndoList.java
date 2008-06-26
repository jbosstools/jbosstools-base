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

public class XUndoList {
    private XUndoManager manager = null;
    private ArrayList<XUndoItem> items = new ArrayList<XUndoItem>(0);

    private int current = 0;
    private boolean isCurrentUndone = false;

    public XUndoList(XUndoManager manager) {
        this.manager = manager;
    }

    public XUndoItem[] items() {
        return items.toArray(new XUndoItem[items.size()]);
    }

    public int getLastUndone() {
        return (isCurrentUndone) ? current : current + 1;
    }

    public void execute(int i) {
        if(i == current) {
            if(isCurrentUndone) manager.redo(); else manager.undo();
            isCurrentUndone = !isCurrentUndone;
        } else if(i < current) {
            if(isCurrentUndone) --current;
            for (int j = current; j >= i; j--) manager.undo();
            isCurrentUndone = true;
        } else if(i > current) {
            if(!isCurrentUndone) ++current;
            for (int j = current; j <= i; j++) manager.redo();
            isCurrentUndone = false;
        }
        current = i;
    }

    void add(XUndoItem item) {
        items.add(item);
    }

    void setCurrent(int current) {
        this.current = current;
        if(current < 0) {
            current = 0;
            isCurrentUndone = true;
        }
    }

}

