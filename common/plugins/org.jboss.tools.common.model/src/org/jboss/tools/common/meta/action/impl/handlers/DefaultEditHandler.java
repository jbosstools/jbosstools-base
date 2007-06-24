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
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.undo.*;

public class DefaultEditHandler extends DefaultCreateHandler {

    public DefaultEditHandler() {}

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object) || data == null) return;
        Properties p = extractProperties(data[0]);
        setOtherProperties(object, p);
        edit(object, p);
    }

    public XEntityData[] getEntityData(XModelObject object) {
        super.getEntityData(object);
        if(data.length > 0) {
            XAttributeData[] ad = data[0].getAttributeData();
            for (int i = 0; i < ad.length; i++) {
                String n = ad[i].getAttribute().getName();
                String v = object.getAttributeValue(n);
                if(v != null) ad[i].setValue(v);
                v = action.getProperty("attribute." + n);
                if(v != null) ad[i].setValue(v);
            }
        }
        return data;
    }

    public static void edit(XModelObject object, Properties p, boolean openTransaction) throws Exception {
        if(!openTransaction) {
            edit(object, p);
            return;
        }
        XUndoManager undo = object.getModel().getUndoManager();
        XTransactionUndo u = new XTransactionUndo("Edit " + DefaultCreateHandler.title(object, false), XTransactionUndo.EDIT);
        undo.addUndoable(u);
        long stamp = object.getTimeStamp();
        try {
            edit(object, p);
            if(stamp == object.getTimeStamp()) undo.rollbackTransactionInProgress();
        } catch (Exception e) {
            undo.rollbackTransactionInProgress();
            throw e;
        } finally {
            u.commit();
        }
    }

    public static void edit(XModelObject object, Properties p) {
        XModelEntity e = object.getModelEntity();
        Enumeration en = p.keys();
        boolean active = object.isActive();
        while(en.hasMoreElements()) {
            String n = (String)en.nextElement();
            if(e.getAttribute(n) == null) continue;
            String v = p.getProperty(n);
            if(!active) object.setAttributeValue(n, v); else
            object.getModel().changeObjectAttribute(object, n, v);
        }
    }

}

