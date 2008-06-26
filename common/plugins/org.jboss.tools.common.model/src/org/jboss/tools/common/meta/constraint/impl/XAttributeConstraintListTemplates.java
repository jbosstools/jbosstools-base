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

import java.io.*;

public class XAttributeConstraintListTemplates extends XAttributeConstraintList {

    public XAttributeConstraintListTemplates() {}

    public String[] getValues() {
        refresh();
        return super.getValues();
    }

    protected void refresh() {
        values.clear();
        String tr = System.getProperty("global.ECOM_HOME") + "/" + "Templates";
        File f = new File(tr);
        File[] fs = f.listFiles();
        if(fs == null) return;
        for (int i = 0; i < fs.length; i++)
          if(fs[i].isDirectory()) values.addElement(fs[i].getName());
    }

    public boolean accepts(String value) {
        refresh();
        return super.accepts(value);
    }
    
    public String getError(String value) {
        return (accepts(value)) ? null : getErrorById("CONSTRAINT_RED_HAT_TEMPLATE_NAME");
    }

}

