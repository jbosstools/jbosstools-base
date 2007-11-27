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
package org.jboss.tools.common.model;

public interface XFilteredTree {
    public void setModel(XModel model);
    public XModelObject getRoot();
    public boolean hasChildren(XModelObject object);
    public XModelObject[] getChildren(XModelObject object);
    public XModelObject getChildAt(XModelObject object, int i);
    public boolean isSelectable(XModelObject object);
    public String getValue(XModelObject object);
    public void setConstraint(Object object);

    public XModelObject find(String value);
    public XModelObject getParent(XModelObject object);
    public String getPath(XModelObject object);
    
    public void dispose();
}

