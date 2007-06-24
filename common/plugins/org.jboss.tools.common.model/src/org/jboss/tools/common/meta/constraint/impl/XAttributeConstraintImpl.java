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
import org.jboss.tools.common.model.util.XBundle;
import org.jboss.tools.common.meta.impl.*;

public class XAttributeConstraintImpl extends XMetaElementImpl implements XAttributeConstraint {
    
    public XAttributeConstraintImpl() {}

    public boolean accepts(String value){
        return true;
    }

    public void load(Element el){
        if(XMetaDataLoader.hasAttribute(el, NAME)) setName(el.getAttribute(NAME));
    }
    
    public String getError(String value) {
        return null;
    }

    public String getCorrectedValue(String value) {
    	//return null;
        return value;
    }

    protected String getErrorById(String id) {
        return XBundle.getInstance().getMessage("model", id);
    }

}

