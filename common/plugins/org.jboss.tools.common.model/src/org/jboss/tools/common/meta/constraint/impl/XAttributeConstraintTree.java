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
package org.jboss.tools.common.meta.constraint.impl;

import org.w3c.dom.*;
import org.jboss.tools.common.meta.constraint.*;

public class XAttributeConstraintTree extends XAttributeConstraintProperties
                                       implements XAttributeConstraintT {
    private String filteredTreeName = "";
    
    public XAttributeConstraintTree() {}

    public void load(Element element) {
        super.load(element);
        NodeList nl = element.getElementsByTagName(VALUE);
        if(nl.getLength() > 0) {
            filteredTreeName = ((Element)nl.item(0)).getAttribute(NAME);
        }
    }

    public String getFilteredTreeName() {
        return filteredTreeName;
    }

}
