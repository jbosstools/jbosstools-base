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

import java.util.*;
import org.jboss.tools.common.model.*;

public class NavigationTree extends DefaultSiftedTree {

    private static final Set<String> escape = new HashSet<String>(2);

    static {
        escape.add("SharableXStudio");
        escape.add("PrimitiveTypes");
//        escape.put("Workspaces", "Workspaces");
    }

    public NavigationTree() {}

	public void dispose() {}

	public XModelObject getRoot() {
        return model.getRoot();
    }

    public XModelObject[] getChildren(XModelObject object) {
        if((object == model.getRoot() || object.getParent() == model.getRoot())
           && !"full".equals(System.getProperty("glory"))) {
            XModelObject[] c = object.getChildren();
            ArrayList<XModelObject> v = new ArrayList<XModelObject>(c.length);
            for (int i = 0; i < c.length; i++) {
                if(escape.contains(c[i].getModelEntity().getName())) continue;
                v.add(c[i]);
            }
            return v.toArray(new XModelObject[v.size()]);
        }
        return object.getChildren();
    }

}

