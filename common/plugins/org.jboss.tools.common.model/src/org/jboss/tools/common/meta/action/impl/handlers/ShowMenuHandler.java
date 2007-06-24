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

import java.util.Properties;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.meta.action.impl.XActionListImpl;
import org.jboss.tools.common.model.XModelObject;

public class ShowMenuHandler extends AbstractHandler {
	SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.action.XMenuInvokerWizard");
	
	public boolean isEnabled(XModelObject object) {
		return wizard != null;
	}
	
    public void executeHandler(XModelObject object, Properties p) throws Exception {
    	XActionList list = getActionList(object);
    	if(list == null) return;
    	p.put("object", object);
    	p.put("actionList", list);
    	wizard.setObject(p);
    	wizard.execute();
    }
    
    XActionList getActionList(XModelObject object) {
    	String listName = action.getProperty("actionList");
    	if(listName != null) {
    		XActionList l = object.getModelEntity().getActionList();
    		if(!(l instanceof XActionListImpl)) return (XActionList)l.getItem(listName);
    		XActionListImpl li = (XActionListImpl)l;
    		return (XActionList)li.getByPath(listName);
    	}
    	return null;
    }

}
