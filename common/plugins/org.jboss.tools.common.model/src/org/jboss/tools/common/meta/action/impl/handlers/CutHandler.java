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
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class CutHandler extends AbstractHandler {
    private XActionHandler copy = null;
    private XActionHandler delete = null;
    private boolean isDefault = false;

    public CutHandler() {}

    private void load(XModelObject object) {
        if(copy != null || object == null) return;
        copy = object.getModelEntity().getActionList().getAction("CopyActions.Copy"); //$NON-NLS-1$
        delete = object.getModelEntity().getActionList().getAction("DeleteActions.Delete"); //$NON-NLS-1$
        isDefault = (copy == null || delete == null);
        if(copy == null) copy = new CopyHandler();
        if(delete == null) delete = new DefaultRemoveHandler();
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
        load(object);
        copy.executeHandler(object, p);
        delete.executeHandler(object, p);
    }

    public boolean getSignificantFlag(XModelObject object) {
        return true;
    }

    public boolean isEnabled(XModelObject object) {
        load(object);
        return copy.isEnabled(object) && delete.isEnabled(object);
    }

    public void setDefaultData(XModelObject object) {}

    public void executeHandler(XModelObject object, XModelObject[] objects, java.util.Properties p) throws XModelException {
        load(object);
        if(isDefault) return;
        copy.executeHandler(object, objects, p);
        delete.executeHandler(object, objects, p);
    }

}
