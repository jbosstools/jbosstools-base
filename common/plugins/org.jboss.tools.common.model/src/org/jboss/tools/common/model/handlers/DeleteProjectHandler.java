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
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.actions.DeleteResourceAction;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class DeleteProjectHandler extends AbstractHandler
{
	public boolean isEnabled(XModelObject object) 
	{
		return object != null;
	}
	 
	public void executeHandler(XModelObject object, Properties p) throws XModelException 
	{
		IProject project = EclipseResourceUtil.getProject(object);
		if (project != null)
		{
			DeleteResourceAction deleteAction = new DeleteResourceAction(ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow());
			deleteAction.selectionChanged(new StructuredSelection(project));
			deleteAction.run();
			if(!project.exists()) {
				XActionInvoker.invoke("Registration.UnregisterInServerXML", FileSystemsHelper.getFileSystems(object.getModel()), null); //$NON-NLS-1$
			}
		}
	}
}
