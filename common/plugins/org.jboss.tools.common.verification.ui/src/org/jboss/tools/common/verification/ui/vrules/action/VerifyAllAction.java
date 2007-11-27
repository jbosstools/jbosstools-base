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
package org.jboss.tools.common.verification.ui.vrules.action;

import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.action.*;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;

public class VerifyAllAction extends AbstractModelActionDelegate {

	public void doRun() throws Exception {
		Properties p = new Properties();
		p.put("shell", window.getShell());
		XActionInvoker.invoke(getActionPath(), object, p);
	}

	protected void safeSelectionChanged(IAction action, ISelection selection) {
		XModelObject adapter = getAdapter(selection);
		if(adapter == null) return;
		object = adapter;
		object = (object == null) ? null : object.getModel().getByPath("FileSystems");
	}
	
	protected boolean computeEnabled() {
		return true;
	}
	
	String path1 = "VerifyActions.StaticActions.VerifyAll";
	String path2 = "ProcessVerifyActions.StaticActions.VerifyAll";
	
	protected String getActionPath() {
		if(object.getModelEntity().getActionList().getAction(path2) != null) return path2;
		return path1;
	}

}
