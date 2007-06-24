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

public class PaletteTree extends DefaultSiftedTree {

    public PaletteTree() {}

	public void dispose() {}

	public XModelObject getRoot() {
        return model.getRoot("XStudio");
    }

    public XModelObject[] getChildren(XModelObject object) {
        return(object == getRoot()) 
             ? new XModelObject[]{
                object.getChildByPath("Icons"),
                object.getChildByPath("Palette")}
             : super.getChildren(object);
    }

}
