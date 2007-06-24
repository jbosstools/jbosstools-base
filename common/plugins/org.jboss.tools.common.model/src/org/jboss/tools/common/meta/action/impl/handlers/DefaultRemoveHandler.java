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

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.undo.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class DefaultRemoveHandler extends AbstractHandler {

    public DefaultRemoveHandler() {}

    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception {
        if(!isEnabled(object)) return;
        if(!isUniqueRequiredChild(object)) {
        	removeFromParent(object);
        } else {
        	cleanUniqueRequiredChild(object);
        }
    }

    public boolean getSignificantFlag(XModelObject object) {
        return true;
    }

    public boolean isEnabled(XModelObject object) {
        if(object == null || !object.isObjectEditable()) return false;
        if("yes".equals(object.getAttributeValue("issystem"))) return false;
        if(object.getParent() == null) return false;
        if(isUniqueRequiredChild(object)) return isUniqueRequiredChildSet(object);
        return true;
    }

    public void setDefaultData(XModelObject object) {}

    public static void removeFromParent(XModelObject object) {
    	if(object == null) return;
        XModelObject parent = object.getParent();
        if(parent == null) return;
        object.removeFromParent();
        XUndoManager undo = DefaultCreateHandler.getUndoManager(parent);
        if(undo != null) {
            parent.getModel().getUndoManager().addUndoable(new XRemoveUndo(parent, object));
        }
        parent.setModified(true);
    }

    private boolean isUniqueRequiredChild(XModelObject object) {
    	XModelObject p = object.getParent();
    	if(p == null) return false;
    	XChild c = p.getModelEntity().getChild(object.getModelEntity().getName());
    	return c != null && c.isRequired() && c.getMaxCount() == 1;
    }
    
    private boolean isUniqueRequiredChildSet(XModelObject object) {
    	XAttribute[] as = object.getModelEntity().getAttributes();
    	for (int i = 0; i < as.length; i++) {
    		String xml = as[i].getXMLName();
    		if(xml == null || xml.length() == 0) continue;
    		String n = as[i].getName();
    		String dv = as[i].getDefaultValue();
    		if(dv == null) dv = "";
    		String v = object.getAttributeValue(n);
    		if(!dv.equals(v)) return true;
    	}
    	XModelObject[] cs = object.getChildren();
    	for (int i = 0; i < cs.length; i++) {
    		if(isEnabled(cs[i])) return true;
    	}
    	return false;
    }
    
    private void cleanUniqueRequiredChild(XModelObject object) throws Exception {
    	XAttribute[] as = object.getModelEntity().getAttributes();
    	for (int i = 0; i < as.length; i++) {
    		String xml = as[i].getXMLName();
    		if(xml == null || xml.length() == 0) continue;
    		String n = as[i].getName();
    		String v = as[i].getDefaultValue();
    		if(v == null) v = "";
    		object.getModel().changeObjectAttribute(object, n, v);
    	}
    	XModelObject[] cs = object.getChildren();
    	for (int i = 0; i < cs.length; i++) {
    		executeHandler(cs[i], null);
    	}
    }

}

