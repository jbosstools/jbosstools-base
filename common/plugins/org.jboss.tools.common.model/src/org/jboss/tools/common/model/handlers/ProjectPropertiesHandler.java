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
package org.jboss.tools.common.model.handlers;

import java.util.*;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.*;
import org.eclipse.jface.window.*;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class ProjectPropertiesHandler extends AbstractHandler {

	public boolean isEnabled(XModelObject object) {
		return object != null && object.getAdapter(IResource.class) != null;
	}

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        return false;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
		IMenuManager m = findWorkbenchMenuManager(IWorkbenchActionConstants.M_PROJECT);
		final IAction action = findAction(m, "org.eclipse.ui.project.properties");
		if(action == null) return;
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				action.run();
			}
		});
	}
	
	public static IMenuManager findWorkbenchMenuManager(String id) {
		Object o = ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
		if(!(o instanceof ApplicationWindow)) return null;
		ApplicationWindow a = (ApplicationWindow)o;
		IContributionItem i = a.getMenuBarManager().find(id);
		return (!(i instanceof IMenuManager)) ? null : (IMenuManager)i;
	}
	
	public static IAction findAction(IMenuManager m, String definitionId) {
		if(m == null) return null;
		IContributionItem[] is = m.getItems();
		for (int k = 0; k < is.length; k++) {
			if(!(is[k] instanceof ActionContributionItem)) continue;
			IAction action = ((ActionContributionItem)is[k]).getAction();
			if(definitionId.equals(action.getActionDefinitionId())) {
				return action;
			}
		}
		return null;
	}

}
