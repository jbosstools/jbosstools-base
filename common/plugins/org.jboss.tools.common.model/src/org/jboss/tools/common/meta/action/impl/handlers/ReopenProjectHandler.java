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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class ReopenProjectHandler extends AbstractHandler {

    private static OpenProjectHandler oph = new OpenProjectHandler();

    public ReopenProjectHandler() {}

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        String s = object.getAttributeValue("name");
        XModelObject ps = object.getModel().getRoot().getChildren("Workspaces")[0];
        if(ps == object) throw new RuntimeException("Handler is implemented in XStudio.");
        XAction a = (XAction)ps.getModelEntity().getActionList().getItem("OpenWorkspace");
        Object d = a.getEntityData(object);
        HUtil.find(d, 0, "redhat project").setValue(s);
        oph.setAction(action);
        oph.setData((XEntityData[])d);
        oph.executeHandler(object, p);
    }

    public boolean isEnabled(XModelObject object) {
        return (object != null);
    }
}