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
package org.jboss.tools.common.model.ui.action;

import java.util.Properties;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class XMenuInvokerWizard implements SpecialWizard {
	XModelObject object;
	XActionList list;
	Control control;	

	public void setObject(Object object) {
		Properties p = (Properties)object;
		this.object = (XModelObject)p.get("object");
		list = (XActionList)p.get("actionList");
		control = (Control)p.get("control");
		if(control == null) {
			control = ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		}
	}

	public int execute() {
		XModelObjectActionList l = new XModelObjectActionList(list, object, null, new Object[]{object});				
		Menu menu = l.createMenu(control);
		menu.setVisible(true);
		return 0;
	}

}
