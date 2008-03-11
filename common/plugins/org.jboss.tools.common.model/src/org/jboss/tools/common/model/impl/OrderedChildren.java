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
package org.jboss.tools.common.model.impl;

import java.util.*;
import org.jboss.tools.common.model.*;

public class OrderedChildren extends RegularChildren {
    protected XModelObject[] list = EMPTY;

    public OrderedChildren() {}

    public boolean areChildrenOrdered() {
    	return true;
    }

    public void clear() {
        super.clear();
        if(list != EMPTY) synchronized (this) {
        	list = EMPTY;
        }
    }
    
    public XModelObject[] getObjects() {
        return list;
    }

    public boolean addObject(XModelObject o) {
        if(!super.addObject(o)) return false;
        synchronized (this) {
        	XModelObject[] ls = new XModelObject[list.length + 1];
        	System.arraycopy(list, 0, ls, 0, list.length);
        	ls[list.length] = o;
        	list = ls;
        }
        return true;
    }

    public boolean removeObject(XModelObject o) {
        if(!super.removeObject(o)) return false;
        int i = getIndex(o);
        if(i >= 0) synchronized (this) {
        	if(list.length == 1) {
        		list = EMPTY;
        	} else {
        		XModelObject[] ls = new XModelObject[list.length - 1];
        		if(i > 0) System.arraycopy(list, 0, ls, 0, i);
        		System.arraycopy(list, i + 1, ls, i, list.length - i - 1);
        		list = ls;
        	}
        }
        return true;
    }

    public synchronized int getIndex(XModelObject o) {
        for (int i = 0; i < list.length; i++) if(o == list[i]) return i;
        return -1;
    }

    public synchronized boolean move(int from, int to) {
        if(list.length == 0) throw new IndexOutOfBoundsException();
        int size = list.length;
        if(to >= size) to = size - 1;
        if(from < 0 || from >= size || from == to) return false;
        XModelObject o = list[from];
        int delta = to > from ? 1 : -1;
        for (int k = from; k != to; k += delta) {
        	list[k] = list[k + delta];        	
        }
        list[to] = o;
        return true;
    }

    public void sort(Comparator<XModelObject> c) {
        if(c == null) c = comparator;
        if(c == null) return;
        Arrays.sort(list, c);
    }

}
