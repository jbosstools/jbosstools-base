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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;

public class SortByNameHandler extends AbstractHandler {

    public boolean isEnabled(XModelObject object) {
        return object != null && object.isObjectEditable() && object instanceof OrderedObjectImpl;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
    	if(!isEnabled(object)) return;
    	OrderedObjectImpl ordered = (OrderedObjectImpl)object;
    	NameComparator c = new NameComparator();
    	c.attribute = action.getProperty("attribute");
    	ordered.sort(c);
    	ordered.setModified(true);
    }
    
    class NameComparator implements Comparator<XModelObject> {
    	String attribute;

		public int compare(XModelObject o1, XModelObject o2) {
			if(attribute == null) return 0;
			String s1 = o1.getAttributeValue(attribute);
			String s2 = o2.getAttributeValue(attribute);
			s1 = (s1 == null) ? "" : s1.toLowerCase();
			s2 = (s2 == null) ? "" : s2.toLowerCase();
			return s1.compareTo(s2);
		}
    	
    }

}
