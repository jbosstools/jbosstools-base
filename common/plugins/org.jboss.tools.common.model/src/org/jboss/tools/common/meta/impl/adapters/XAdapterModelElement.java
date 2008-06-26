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
package org.jboss.tools.common.meta.impl.adapters;

import org.w3c.dom.*;
import org.jboss.tools.common.meta.constraint.*;

public class XAdapterModelElement extends XAdapter {
    private String xmlname = "";

    public XAdapterModelElement() {}

    public String getProperty(XProperty object) {
        return object.get(xmlname);
    }

    public void setProperty(XProperty object, String value) {
        if(constraint.accepts(value)) object.set(xmlname, value);
        else {
            value = constraint.getCorrectedValue(value);
            if(value != null) object.set(xmlname, value);
        }
    }

    public String getXMLname() {
        return xmlname;
    }

    public void load(Element element) {
        super.load(element);
        xmlname = element.getAttribute(XML_NAME);
    }

}
