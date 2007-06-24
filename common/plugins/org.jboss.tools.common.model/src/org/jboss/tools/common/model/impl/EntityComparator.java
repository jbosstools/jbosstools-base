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
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.meta.*;

public class EntityComparator implements Comparator<XModelObject> {
    private static HashMap<String,EntityComparator> comparators = new HashMap<String,EntityComparator>();

    public static EntityComparator getComparator(XModelEntity entity) {
        EntityComparator c = (EntityComparator)comparators.get(entity.getName());
        if(c == null) {
            c = new EntityComparator(entity.getChildren());
            comparators.put(entity.getName(), c);
        }
        return c;
    }

    private Hashtable<String,Integer> entities = null;
    boolean ignoreCase = false;

    public EntityComparator(XChild[] c) {
        entities = new Hashtable<String,Integer>(c.length);
        for (int i = 0; i < c.length; i++) {
            entities.put(c[i].getName(), new Integer(i * 100));
        }
    }
    
    public void setIgnoreCase(boolean b) {
    	ignoreCase = b;
    }

    public int compare(XModelObject s1, XModelObject s2) {
        int i1 = getEntityRange(s1);
        int i2 = getEntityRange(s2);
        if(i1 != i2) return (i1 - i2);
        if(ignoreCase) {
        	String p1 = s1.getPathPart();
        	String p2 = s2.getPathPart();
        	p1 = (p1 == null) ? "" : p1.toLowerCase();
        	p2 = (p2 == null) ? "" : p2.toLowerCase();
        	return p1.compareTo(p2);
        }
        return s1.getPathPart().compareTo(s2.getPathPart());
    }

    private int getEntityRange(XModelObject s) {
        Integer i = (Integer)entities.get(s.getModelEntity().getName());
        return (i == null) ? 1000 : i.intValue();
    }

    public boolean equals(Object obj) {
        return obj == this;
    }

}

