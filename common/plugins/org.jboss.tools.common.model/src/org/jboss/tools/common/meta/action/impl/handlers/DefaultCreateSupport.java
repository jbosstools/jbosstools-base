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
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class DefaultCreateSupport extends SpecialWizardSupport {
	
	public boolean isEnabled(XModelObject object) {
		return (object != null && object.isObjectEditable());
	}

	protected String getEntityName() {
		String n = action.getProperty("entity");
		return (n == null) ? getEntityData()[0].getModelEntity().getName() : n;
	}

	public void reset() {
		String entity = getEntityName();
		XModelObject[] cs = getTarget().getChildren(entity);
		XChild c = getTarget().getModelEntity().getChild(entity);
		int limit = c.getMaxCount();
		if(c != null && limit < Integer.MAX_VALUE && cs.length >= limit) {
			ServiceDialog d = getTarget().getModel().getService();
			d.showDialog("Warning", "The limit of " + limit + " children is achieved.", new String[]{OK}, null, ServiceDialog.MESSAGE);
			setFinished(true);
		}		
	}

	public void action(String name) throws Exception {
		if(OK.equals(name)) {
			finish();
			setFinished(true);
		} else if(CANCEL.equals(name)) {
			setFinished(true);
		} else if(HELP.equals(name)) {
			help();
		}
	}
	
	protected void finish() throws Exception {
		String entity = getEntityName();
		Properties p = extractStepData(0);
		XModelObject c = XModelObjectLoaderUtil.createValidObject(getTarget().getModel(), entity, p);
		DefaultCreateHandler.addCreatedObject(getTarget(), c, getProperties());
	}

}
