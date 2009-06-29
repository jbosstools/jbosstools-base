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

import org.jboss.tools.common.MethodNotImplementedException;
import org.jboss.tools.common.model.XFilteredTree;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;

public abstract class DefaultSiftedTree implements XFilteredTree {

    protected XModel model = null;

    public DefaultSiftedTree() {}

    abstract public void dispose();

    public void setModel(XModel model) {
        this.model = model;
    }

    public XModelObject getChildAt(XModelObject object, int i) {
        XModelObject[] c = getChildren(object);
        return (i < 0 || i >= c.length) ? null : c[i];
    }

    public boolean hasChildren(XModelObject object) {
        return object.hasChildren();
    }

    public XModelObject[] getChildren(XModelObject object) {
        return object.getChildren();
    }

    public boolean isSelectable(XModelObject object) {
        throw new MethodNotImplementedException("Not implemented"); //$NON-NLS-1$
    }

    public String getValue(XModelObject object) {
        throw new MethodNotImplementedException("Not implemented"); //$NON-NLS-1$
    }

    public void setConstraint(Object object) {}

    public XModelObject find(String value) {
        throw new MethodNotImplementedException("Not implemented"); //$NON-NLS-1$
    }

    public XModelObject getParent(XModelObject object) {
        return object.getParent();
    }

    public String getPath(XModelObject object) {
        return object.getPath();
    }
}

