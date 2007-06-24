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

import java.lang.reflect.*;

import org.eclipse.core.runtime.Status;

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class UndoRedoHandler extends AbstractHandler {
    private String wizardname = (System.getProperty("testmodel") == null)
                  ? "XXX.UndoRedoWizard"
                  : "XXX.UndoRedoWizard";

    public UndoRedoHandler() {}

    public XEntityData[] getEntityData(XModelObject object) {
        return new XEntityData[0];
    }

    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception {
        if(!isEnabled(object)) return;
        call(object.getModel());
    }

    public boolean isEnabled(XModelObject object) {
        if(object == null) return false;
        return object.getModel().getRoot().isModified();
///        return object.getModel().getUndoManager().canUndo() ||
///               object.getModel().getUndoManager().canRedo();
    }

    private int call(XModel model) {
        try {
            Object o = ModelFeatureFactory.getInstance().createFeatureInstance(wizardname);
            Class[] types = new Class[]{String.class, XModel.class};
            Method m = o.getClass().getMethod("showDialog", types);
            Object r = m.invoke(o, new Object[]{"Undo/Redo History", model});
            return ((Integer)r).intValue();
        } catch (Exception e) {
			ModelPlugin.getDefault().getLog().log(new Status(Status.ERROR, ModelPlugin.PLUGIN_ID, Status.OK, "Model warning: Cannot load class " + wizardname + ". Or invoke his method 'showDialog'",e));
            return -1;
        }
    }

}
