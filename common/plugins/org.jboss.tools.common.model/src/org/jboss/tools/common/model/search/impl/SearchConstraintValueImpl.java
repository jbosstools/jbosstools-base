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
package org.jboss.tools.common.model.search.impl;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;

public class SearchConstraintValueImpl extends SearchConstraintImpl {
	private static final long serialVersionUID = 1L;
	protected String propertyname;
    protected String propertyvalue;
    protected boolean ignorecase;
    protected int equality;

    public SearchConstraintValueImpl() {}

    public boolean accepts(XModelObject object) {
        if("*".equals(propertyname)) {
            XAttribute[] as = object.getModelEntity().getAttributes();
            for (int i = 0; i < as.length; i++) {
                if(matches(object, as[i].getName()) ? !not : not) return true;
            }
            return false;
        }
        return matches(object, propertyname);
    }

    private boolean matches(XModelObject object, String name) {
        String value = object.getAttributeValue(name);
        return (matches(value, propertyvalue)) ? !not : not;
    }

    protected boolean matches(String value, String v) {
        if(value == null) return false;
        if(ignorecase) value = value.toLowerCase();
        if(equality == 0) return value.equals(v);
        if(equality > 0) return value.indexOf(v) >= 0;
        else return v.indexOf(value) >= 0;
    }

    public void prepare() {
        propertyname = getAttributeValue("property name");
        propertyvalue = getAttributeValue("text to find");
        not = "true".equals(getAttributeValue("not"));
        ignorecase = "true".equals(getAttributeValue("ignore case"));
        if(ignorecase) propertyvalue = propertyvalue.toLowerCase();
        String eq = getAttributeValue("equality");
        equality = ("coincides".equals(eq)) ? 0 :
                   ("contains".equals(eq)) ? 1 : -1;
    }

}
