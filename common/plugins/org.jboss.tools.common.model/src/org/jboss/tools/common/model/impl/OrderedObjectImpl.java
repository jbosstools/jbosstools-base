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

import java.util.Comparator;

import org.jboss.tools.common.model.*;

public class OrderedObjectImpl extends RegularObjectImpl {
    private static final long serialVersionUID = 9157356707044480881L;

    public OrderedObjectImpl() {}

    protected RegularChildren createChildren() {
        return new OrderedChildren();
    }
    
    public void sort(Comparator<XModelObject> c) {
    	XModelObject[] cs = (XModelObject[])getChildren().clone();
		OrderedChildren oc = (OrderedChildren)children;
		oc.sort(c);
		XModelObject[] cs2 = getChildren();
		if(!equalArrays(cs, cs2)) {
			changeTimeStamp();
			((XModelImpl)getModel()).fireStructureChanged(this);
			setModified(true);    	
		}
    }
    
    private boolean equalArrays(Object[] o1, Object[] o2) {
    	if(o1.length != o2.length) return false;
    	for (int i = 0; i < o1.length; i++) if(o1[i] != o2[i]) return false;
    	return true;
    }

}

