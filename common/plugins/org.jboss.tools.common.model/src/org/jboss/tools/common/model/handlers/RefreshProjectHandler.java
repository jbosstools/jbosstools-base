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

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.actions.RefreshAction;

import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;


public class RefreshProjectHandler extends AbstractHandler
{
	public boolean isEnabled(XModelObject object) 
	{
		return object != null;
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception 
	{
		IProject project = (IProject)object.getModel().getProperties().get("project");
		if (project != null)
		{
			RefreshAction refreshAction = new RefreshAction(ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell());
			refreshAction.selectionChanged(new StructuredSelection(project));
			refreshAction.run();
		}
	}
}
