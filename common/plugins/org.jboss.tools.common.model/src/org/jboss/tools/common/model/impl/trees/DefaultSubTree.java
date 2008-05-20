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
package org.jboss.tools.common.model.impl.trees;

import org.jboss.tools.common.model.*;

public class DefaultSubTree extends DefaultSiftedTree {
    protected XModelObject root = null;

    public DefaultSubTree() {}

	public void dispose() {}

	public void setModel(XModel model) {
        super.setModel(model);
        root = model.getRoot();
    }

    public XModelObject getRoot() {
        return root;
    }

    public void setConstraint(Object object) {
        if(object instanceof XModelObject)
        root = (XModelObject)object; 
    }

    public boolean isSelectable(XModelObject object) {
        return true;
    }

    public String getValue(XModelObject object) {
        if(object == root) return "";
        String sr = root.getPath() + "/";
        String so = object.getPath();
        return (so.startsWith(sr)) ? so.substring(sr.length()) : so;
    }

    public XModelObject find(String value) {
        return ("".equals(value)) ? root : root.getChildByPath(value);
    }


} 