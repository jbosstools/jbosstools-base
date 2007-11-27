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
package org.jboss.tools.common.model.impl;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;

public class OrderedByEntityChildren extends GroupOrderedChildren {
    protected String[] children = null;

    protected void loadEntities(XModelObject o) {
        if(children == null) {
            XModelObject p = o.getParent();
            if(p == null) return;
            XChild[] cs = p.getModelEntity().getChildren();
            children = new String[cs.length];
            for (int i = 0; i < cs.length; i++) children[i] = cs[i].getName();
        }
        if(limits.length != children.length) limits = new int[children.length];
    }

    protected int getGroup(XModelObject o) {
        loadEntities(o);
        return getEntityIndex(o.getModelEntity().getName());
    }

	protected int getEntityIndex(String s) {
        if(children == null) return 0;
        for (int i = 0; i < children.length; i++)
          if(children[i].equals(s)) return i;
        return 0;
    }

}
