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
package org.jboss.tools.common.meta.constraint;

import org.w3c.dom.*;
import org.jboss.tools.common.meta.impl.*;

public class XAdapter extends XMetaElementImpl {
    protected XAttributeConstraint constraint = null;
    protected boolean visibility = true;
    protected String defaultvalue = "";

    public XAdapter() {}

    public String getProperty(XProperty object) {
        return object.get(getName()); //override
    }

    public void setProperty(XProperty object, String value) {
        if(constraint.accepts(value)) object.set(getName(), value); //override
        else {
            value = constraint.getCorrectedValue(value);
            if(value != null) object.set(getName(), value);
        }
    }

    public XAttributeConstraint getConstraint() {
        return constraint;
    }

    public void setConstraint(XAttributeConstraint constraint) {
        this.constraint = constraint;
    }

    public boolean isVisible() {
        return visibility;
    }

    public String getDefaultValue() {
        return defaultvalue;
    }

    public void reload(Element element) {
        load(element);
    }

    public void load(Element element) {
        setName(element.getAttribute(NAME));
        defaultvalue = element.getAttribute(DEFAULT_VALUE);
        visibility = !"false".equals(element.getAttribute(VISIBLE));
    }
}

