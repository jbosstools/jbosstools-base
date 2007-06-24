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
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;

public class CloseProjectHandler extends AbstractHandler {
//    private ReopenProjectHandler rph = new ReopenProjectHandler();

    public CloseProjectHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (object != null && object.isActive());
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        XModel m = object.getModel();
        if(!NewProjectHandler.save(m, action)) return;
        removeWorkspace(object);
        m.getProperties().remove(XModelConstants.WORKSPACE);
        m.getProperties().remove("global.ECOM_DATA");
        m.load();
    }

    private void removeWorkspace(XModelObject object) {
        object.getParent().set("OPEN", "no");
        object.setModified(true);
        object.removeFromParent();
        object.getModel().save();
    }

}
