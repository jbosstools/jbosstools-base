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

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.util.XModelObjectUtil;

public class OpenedProjectsImpl extends OrderedObjectImpl {
	private static final long serialVersionUID = 1L;
    static final int MAX_PROJECT_COUNT = 10;

    public OpenedProjectsImpl() {}

    public void last(String name) {
        XModelObject c = getChildByFile(name);
        boolean reopen = (c != null);
        if(!reopen) {
            Properties p = new Properties();
            p.setProperty("name", name);
            c = getModel().createModelObject("Workspace", p);
            addChild_0(c);
        }
        int i = getIndexOfChild(c);
        if(reopen && i == 0) return;
        while(i > 0) { children.move(i, i - 1); --i; }
        if(reopen) {
            fireStructureChanged(XModelTreeEvent.STRUCTURE_CHANGED, null);
        } else {
            fireStructureChanged(XModelTreeEvent.CHILD_ADDED, c);
        }
        c.setModified(true);
        XModelObject[] os = children.getObjects();
        if(os.length > MAX_PROJECT_COUNT)
          for (int q = MAX_PROJECT_COUNT; q < os.length; q++) os[q].removeFromParent();
        set("OPEN", "yes");
    }

    public XModelObject getChildByFile(String name) {
        if(name == null) return null;
        name = name.toLowerCase().replace('\\', '/');
        XModelObject[] os = children.getObjects();
        for (int i = 0; i < os.length; i++) {
            if(name.equals(XModelObjectUtil.getExpandedValue(os[i], "name", null).toLowerCase().replace('\\', '/'))) return os[i];
        }
        return null;
    }

}
