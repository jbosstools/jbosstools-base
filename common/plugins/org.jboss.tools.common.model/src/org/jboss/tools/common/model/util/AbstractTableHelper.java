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
package org.jboss.tools.common.model.util;

import org.jboss.tools.common.model.*;

public abstract class AbstractTableHelper {
    protected XModelObject object = null;

    public AbstractTableHelper() {}

    public void setModelObject(XModelObject object) {
        this.object = object;
    }

    public XModelObject getModelObject() {
        return object;
    }

    public String[] getHeader() {
        return new String[0];
    }
    
    public String[] getVisibleHeader() {
    	return getHeader();
    }

    public int size() {
        return (object == null) ? 0 : object.getChildren().length;
    }

    public XModelObject getModelObject(int r) {
        if(object == null) return null;
        XModelObject[] cs = object.getChildren();
        return (r < 0 || r >= cs.length) ? null : cs[r];
    }

    public XModelObject getModelObject(int r, int c) {
        return getModelObject(r);
    }

    public String getValueAt(int r, int c) {
        XModelObject f = getModelObject(r);
        return (f == null) ? "" : f.getAttributeValue(getHeader()[c]);
    }

    public void setValueAt(int r, int c, String value) {
        XModelObject f = getModelObject(r);
        if(f == null) return;
        f.getModel().changeObjectAttribute(f, getHeader()[c], value);
    } 

}

