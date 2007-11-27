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
package org.jboss.tools.common.meta.action.impl;

import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.undo.*;

public class AbstractHandler implements XActionHandler {
    protected XAction action;
    protected XEntityData[] data;

    public AbstractHandler() {}

    public final void setAction(XAction action) {
        this.action = action;
    }

    public XEntityData[] getEntityData(XModelObject object) {
        setDefaultData(object);
        return data;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {}

    public boolean getSignificantFlag(XModelObject object) {
        return false;
    }

    public boolean isEnabled(XModelObject object) {
        return false;
    }

    public boolean hide(boolean enabled) {
        return ((XActionImpl)action).hide0(enabled);
    }

    public void setDefaultData(XModelObject object) {
        setDefaultValues();
    }

    public void setDefaultValues() {
        for (int i = 0; i < data.length; i++)
          ((XEntityDataImpl)data[i]).setDefaultValues();
    }

    public void setData(XEntityData[] data) {
        this.data = data;
    }

    protected void checkEnvironment(Object environment) {}

    //multi//

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        if(object == null || objects == null || objects.length < 2) return isEnabled(object);
        if(action.getWizardClassName() != null && action.getWizardClassName().length() > 0) return false;
        XModelObject p = object.getParent();
        XModelEntity entity = object.getModelEntity();
        for (int i = 0; i < objects.length; i++) {
            if(p != objects[i].getParent() 
            	&& (object.getFileType() != objects[i].getFileType() || object.getFileType() != XModelObject.FILE) 
            	&& !ignoreDifferentParents(objects[i].getModelEntity().getName(), entity.getName())) {
                return false;
            }
        }
        String path = action.getPath();
        for (int i = 0; i < objects.length; i++) {
            XModelEntity ent = objects[i].getModelEntity();
            if(ent == entity) {
                if(!action.isEnabled(objects[i])) return false;
            }
            XAction a = (XAction)((XActionListImpl)ent.getActionList()).getByPath(path);
            if(a == null || (action.getWizardClassName() != null && a.getWizardClassName().length() > 0)
               || !a.isEnabled(objects[i])) return false;
        }
        return action.isEnabled(object);
    }
    
    protected boolean ignoreDifferentParents(String entity1, String entity2) {
        return entity1.equals(entity2);
    }

    public void executeHandler(XModelObject object, XModelObject[] objects, java.util.Properties p) throws Exception {
        if(!isEnabled(object, objects)) return;
        if(object == null || objects == null || objects.length < 2) {
            executeHandler(object, p);
            return;
        }
        String path = action.getPath();
        XModelEntity entity = object.getModelEntity();
        for (int i = 0; i < objects.length; i++) {
            XModelEntity ent = objects[i].getModelEntity();
            if(ent == entity) {
                executeHandler(objects[i], p);
                continue;
            }
            XAction a = (XAction)((XActionListImpl) ent.getActionList()).getByPath(path);
            if(a == null || !a.isEnabled(objects[i])) continue;
            mergeEntityData(data, a.getEntityData(objects[i]));
            a.executeHandler(objects[i], p);
        }
    }

    protected void mergeEntityData(XEntityData[] source, XEntityData[] target) {
        int ic = source.length;
        if(target.length < ic) ic = target.length;
        for (int i = 0; i < ic; i++) {
            XAttributeData[] ds = source[i].getAttributeData();
            XAttributeData[] dt = target[i].getAttributeData();
            mergeAttributeData(ds, dt);
        }
    }
    protected void mergeAttributeData(XAttributeData[] source, XAttributeData[] target) {
        int ic = source.length;
        if(target.length < ic) ic = target.length;
        for (int i = 0; i < ic; i++) {
            target[i].setValue(source[i].getValue());
        }
    }

    // dirty hacks

    /*
     * Should be called only from overrided method "isEnabled",
     * being aware that wizard value specified in meta
     * gets meaningless.
     */

    protected final void setWizardClassName(String wizard) {
        ((XActionImpl)action).setWizardClassName(wizard);
    }
    
	public final void executeInTransaction(XModelObject object, Properties p, String transactionName, int transactionKind) throws Exception {
		if(!isEnabled(object)) return;
		XUndoManager undo = object.getModel().getUndoManager();
		XTransactionUndo u = new XTransactionUndo(transactionName, transactionKind);
		undo.addUndoable(u);
		try {
			transaction(object, p);
		} catch (Exception e) {
			undo.rollbackTransactionInProgress();
			throw e;
		} finally {
			u.commit();
		}
	}
	
	protected void transaction(XModelObject object, Properties p) throws Exception {
	}

}

