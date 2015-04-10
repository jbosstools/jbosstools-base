/*******************************************************************************
 * Copyright (c) 2015 Red Hat 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     JBoss by Red Hat
 *******************************************************************************/
package org.jboss.tools.runtime.core.model.internal;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.jboss.tools.foundation.core.tasks.TaskModel;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeWorkflowConstants;
import org.jboss.tools.runtime.core.model.IRuntimeInstaller;
import org.jboss.tools.runtime.core.util.internal.DownloadRuntimeOperationUtility;

public class ExtractionRuntimeInstaller implements IRuntimeInstaller {

	public static final String ID = IRuntimeInstaller.EXTRACT_INSTALLER;
	
	@Override
	public IStatus installRuntime(DownloadRuntime downloadRuntime, 
			String selectedDirectory, String destinationDirectory,
			boolean deleteOnExit, TaskModel taskModel, IProgressMonitor monitor) {
		
		String user = (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.USERNAME_KEY);
		String pass = (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.PASSWORD_KEY);
		
		monitor.beginTask("Download '" + downloadRuntime.getName() + "' ...", 100);//$NON-NLS-1$ //$NON-NLS-2$
		monitor.worked(1);
		return new DownloadRuntimeOperationUtility().downloadAndUnzip(selectedDirectory, destinationDirectory, 
				getDownloadUrl(downloadRuntime, taskModel), deleteOnExit, user, pass, taskModel, new SubProgressMonitor(monitor, 99));
	}

	private String getDownloadUrl(DownloadRuntime downloadRuntime, TaskModel taskModel) {
		if( downloadRuntime != null ) {
			String dlUrl = downloadRuntime.getUrl();
			if( dlUrl == null ) {
				return (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.DL_RUNTIME_URL);
			}
			return dlUrl;
		}
		return null;
	}
}
