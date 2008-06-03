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

public class GroupOrderedChildren extends RegularChildren {
    protected XModelObject[] list = EMPTY;
    int[] limits = new int[getGroupCount()];

    public GroupOrderedChildren() {}

    public boolean areChildrenOrdered() {
    	return true;
    }

    protected int getGroupCount() {
        return 2;
    }

    protected int getGroup(XModelObject o) {
        return 0;
    }

    public void clear() {
        super.clear();
        if(list != EMPTY) synchronized (this) {
        	list = EMPTY;
        	for (int i = 0; i < limits.length; i++) limits[i] = 0;
        }
    }

    public XModelObject[] getObjects() {
        return list;
    }

    public boolean addObject(XModelObject o) {
        if(!super.addObject(o)) return false;
        int g = getGroup(o);
        int i = limits[g];
        synchronized (this) {
        	XModelObject[] ls = new XModelObject[list.length + 1];
        	System.arraycopy(list, 0, ls, 0, i);
        	ls[i] = o;
        	System.arraycopy(list, i, ls, i + 1, list.length - i);
        	for (int k = g; k < limits.length; k++) limits[k]++;
        	list = ls;
        }
        return true;
    }

    public boolean removeObject(XModelObject o) {
        if(!super.removeObject(o)) return false;
        int i = getIndex(o);
        if(i >= 0) {
        	if(list.length == 1) {
        		list = EMPTY;
        	} else {
        		XModelObject[] ls = new XModelObject[list.length - 1];
        		if(i > 0) System.arraycopy(list, 0, ls, 0, i);
        		System.arraycopy(list, i + 1, ls, i, list.length - i - 1);
        		list = ls;
        	}
    		for (int g = 0; g < limits.length; g++) {
    			if(limits[g] > i) limits[g]--;
    		}
        }
        return true;
    }

    private int getGroup(int i) {
        for (int g = 0; g < limits.length; g++) {
            if(limits[g] > i) return g;
        }
        return -1;
    }

    public int getIndex(XModelObject o) {
        for (int i = 0; i < list.length; i++) {
        	if(o == list[i]) return i;
        }
        return -1;
    }

    public boolean move(int from, int to) {
        int c = list.length;
        if(c == 0) return false;
        if(to >= c) to = c - 1;
        int g = getGroup(from);
        if(g < 0) return false;
        int goff = (g == 0) ? 0 : limits[g - 1];
        int gend = limits[g];
        if(to < goff) to = goff; else if(to >= gend) to = gend - 1;
        if(from < 0 || from >= c || from == to) return false;
        XModelObject o = list[from];
        int delta = to > from ? 1 : -1;
        for (int k = from; k != to; k += delta) {
        	list[k] = list[k + delta];        	
        }
        list[to] = o;
        return true;
    }

    public void sort(Comparator c) {}

}
