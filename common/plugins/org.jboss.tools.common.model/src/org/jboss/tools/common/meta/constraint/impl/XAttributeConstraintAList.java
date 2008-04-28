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

import java.util.*;
import org.w3c.dom.*;
import org.jboss.tools.common.meta.constraint.*;

public abstract class XAttributeConstraintAList
                      extends XAttributeConstraintImpl
                      implements XAttributeConstraintL {

    protected Vector<String> values = new Vector<String>();

    public XAttributeConstraintAList() {}

    public void load(Element element) {
        super.load(element);
        NodeList nl = element.getElementsByTagName(VALUE);
        for (int i = 0; i < nl.getLength(); i++) {
            Element c = (Element)nl.item(i);
            values.addElement(c.getAttribute(NAME));
        }
    }

    public String[] getValues() {
        return values.toArray(new String[0]);
    }

    public int indexOf(String value) {
        return values.indexOf(value);
    }

    public String valueAt(int i) {
        return (i < 0 || i >= values.size()) ? null : (String)values.elementAt(i);
    }

    /*
     *  Can be used only by handlers for helper attributes.
     */

    public void setValues(String[] _values) {
        values.clear();
        if(_values == null) return;
        for (int i = 0; i < _values.length; i++)
          values.addElement(_values[i]);
    }

}
