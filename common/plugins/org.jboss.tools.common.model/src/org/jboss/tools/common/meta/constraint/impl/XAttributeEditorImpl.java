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
import org.jboss.tools.common.meta.constraint.XAttributeEditor;
import org.jboss.tools.common.meta.impl.*;

public class XAttributeEditorImpl extends XMetaElementImpl implements XAttributeEditor {
    protected String editor = null;
    protected String viewer = null;
    private Object context = null;

    public XAttributeEditorImpl() {}

    public void setName(String name) {
        if(name == null || name.trim().length() == 0) this.name = "Text";
        else this.name = name;
    }

    public String getEditorClassName() {
        if (editor == null) setDefaultClassName();
        return editor;
    }

    public String getViewerClassName() {
        if(viewer != null && viewer.startsWith("%")) findViewerName();
        return viewer;
    }

    private void findViewerName() {
        try {
            viewer = viewer.substring(1, viewer.length() - 1);
            viewer = getMetaModel().getMapping("AttributeEditor").getValue(viewer);
        } catch (Exception e) {
        	//ignore
            viewer = null;
        }
    }


    protected void setDefaultClassName() {
        try {
            editor = getMetaModel().getMapping("AttributeEditor").getValue(getName());
        } catch (Exception e) {
        	//ignore
        }
    }

    public void load(Element element) {
        editor = (!XMetaDataLoader.hasAttribute(element, EDITOR_CLASSNAME)) ? null :
                     element.getAttribute(EDITOR_CLASSNAME);
        viewer = (!XMetaDataLoader.hasAttribute(element, EDITOR_VIEWERNAME)) ? null :
                     element.getAttribute(EDITOR_VIEWERNAME);
    }

    public void setContext(Object context) {
        this.context = context;
    }

    public Object getContext() {
        return context;
    }

}

