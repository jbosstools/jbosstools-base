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
    protected List<XModelObject> alist = new ArrayList<XModelObject>();

    public OrderedChildren() {}

    public boolean areChildrenOrdered() {
    	return true;
    }

    public void clear() {
        super.clear();
        if(size() > 0) synchronized (this) {
        	alist.clear();
        	list = EMPTY;
        }
    }
    
    public XModelObject[] getObjects() {
    	if(list == null) synchronized (this) {
    		list = alist.toArray(new XModelObject[0]);
    	}
        return list;
    }

    public boolean addObject(XModelObject o) {
        if(!super.addObject(o)) return false;
        synchronized (this) {
        	alist.add(o);
        	list = null;
        }
        return true;
    }

    public boolean removeObject(XModelObject o) {
        if(!super.removeObject(o)) return false;
        int i = getIndex(o);
        if(i >= 0) synchronized (this) {
        	if(alist.size() == 1) {
        		list = EMPTY;
        		alist.clear();
        	} else {
        		alist.remove(i);
            	list = null;
        	}
        }
        return true;
    }

    public synchronized int getIndex(XModelObject o) {
    	return alist.indexOf(o);
    }

    public synchronized boolean move(int from, int to) {
        int size = size();
        if(size == 0) throw new IndexOutOfBoundsException();
        if(to >= size) to = size - 1;
        if(from < 0 || from >= size || from == to) return false;
        XModelObject o = alist.get(from);
        int delta = to > from ? 1 : -1;
        for (int k = from; k != to; k += delta) {
        	alist.set(k, alist.get(k + delta));
        	if(list != null) list[k] = list[k + delta];
        }
        alist.set(to, o);
    	if(list != null) list[to] = o;
        return true;
    }

    public void sort(Comparator<XModelObject> c) {
        if(c == null) c = comparator;
        if(c == null) return;
        getObjects();
        synchronized(this) {
        	Arrays.sort(list, c);
        	alist = asList(list);
        }
    }

	public void replaceChildren(XModelObject[] objects) {
		super.replaceChildren(objects);
		synchronized (this) {
			alist = asList(objects);
			list = null;
		}
	}

	private List<XModelObject> asList(XModelObject[] objects) {
		ArrayList<XModelObject> n = new ArrayList<XModelObject>();
		for (int i = 0; i < objects.length; i++) {
			n.add(objects[i]);
		}
		return n;
	}

}
