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
package org.jboss.tools.runtime.ui.internal.wizard;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.core.tasks.TaskModel;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeWorkflowConstants;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.internal.DownloadRuntimeOperationUtility;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.dialogs.SearchRuntimePathDialog;

/**
 * Mixed class of core+ui to initiate the download, unzipping, 
 * and runtime creation for a downloaded runtime. 
 */
public class DownloadRuntimeOperationUIUtility extends DownloadRuntimeOperationUtility{

	public static IStatus createRuntimes(String directory, IProgressMonitor monitor) {
		IStatus ret = Status.OK_STATUS;
		monitor.subTask("Creating runtime from location " + directory); //$NON-NLS-1$
		final RuntimePath runtimePath = new RuntimePath(directory);
		List<RuntimeDefinition> runtimeDefinitions = RuntimeInitializerUtil.createRuntimeDefinitions(runtimePath, monitor);
		RuntimeUIActivator.getDefault().getModel().addRuntimePath(runtimePath);
		if (runtimeDefinitions.size() == 0) {
			return new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, NLS.bind(Messages.DownloadRuntimesSecondPage_No_runtime_server_found, directory));
		} else if (runtimeDefinitions.size() > 1) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					SearchRuntimePathDialog.launchSearchRuntimePathDialog(
							Display.getDefault().getActiveShell(),
							RuntimeUIActivator.getDefault().getModel().getRuntimePaths(), false, 7);
				}
			});
		} else /* size == 1 */{
			ret = RuntimeInitializerUtil.initializeRuntimes(runtimeDefinitions);
		}
		monitor.done();
		return ret;
	}

	
	public static IOverwrite createOverwriteFileQuery() {
		IOverwrite overwriteQuery = new IOverwrite() {
			public int overwrite(File file) {
				final String msg = NLS.bind(Messages.DownloadRuntimesSecondPage_The_file_already_exists, file.getAbsolutePath()); 
				final String[] options = { IDialogConstants.YES_LABEL,
						IDialogConstants.YES_TO_ALL_LABEL,
						IDialogConstants.NO_LABEL,
						IDialogConstants.NO_TO_ALL_LABEL,
						IDialogConstants.CANCEL_LABEL };
				final int[] retVal = new int[1];
				Display.getDefault().syncExec(new Runnable() {
					public void run() {
						Shell shell = PlatformUI.getWorkbench().getModalDialogShellProvider().getShell();
						MessageDialog dialog = new MessageDialog(shell, Messages.DownloadRuntimesSecondPage_Question,
								null, msg, MessageDialog.QUESTION, options, 0) {
							protected int getShellStyle() {
								return super.getShellStyle() | SWT.SHEET;
							}
						};
						dialog.open();
						retVal[0] = dialog.getReturnCode();
					}
				});
				return retVal[0];
			}
		};
		return overwriteQuery;
	}
	

}
