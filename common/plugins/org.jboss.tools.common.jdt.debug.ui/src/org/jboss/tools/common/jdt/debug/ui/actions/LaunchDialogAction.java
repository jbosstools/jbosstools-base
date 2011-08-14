/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.ui.actions;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.internal.ui.DebugUIPlugin;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationManager;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationsDialog;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchGroupExtension;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;

/**
 * 
 * @author snjeza
 *
 */
public class LaunchDialogAction extends Action {

	public void run() {
		LaunchConfigurationManager lcManager = DebugUIPlugin.getDefault().getLaunchConfigurationManager();
		LaunchGroupExtension group = lcManager.getLaunchGroup(RemoteDebugActivator.LAUNCH_CATEGORY);
		LaunchConfigurationsDialog dialog = new LaunchConfigurationsDialog(getShell(), group);
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
		ILaunchConfiguration config = RemoteDebugActivator.getDefault().getDefaultLaunchConfiguration();
		ILaunchConfigurationWorkingCopy wc = null;
		try {
			if (config == null) {
				wc = RemoteDebugActivator.createNewLaunchConfiguration(type);
			} else {
				wc = config.getWorkingCopy();
			}
		} catch (CoreException e) {
			RemoteDebugUIActivator.log(e);
			RemoteDebugUIActivator.displayLaunchError(getShell(), config, e);
		}
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		ISelection currentSelection  = page.getSelection();
		if (currentSelection instanceof ITreeSelection) {
			ITreeSelection selection = (ITreeSelection) currentSelection;
			Object object = selection.getFirstElement();
			if (!(object instanceof IJavaElement)
					&& object instanceof IAdaptable) {
				object = ((IAdaptable) object)
						.getAdapter(IJavaElement.class);
			}
			if (object instanceof IJavaElement) {
				IJavaElement javaElement = (IJavaElement) object;
				IJavaProject javaProject = javaElement.getJavaProject();
				if (javaProject != null) {
					try {
						wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, javaProject.getElementName());
						RemoteDebugUIActivator.getDefault().addSelectedProjects(wc, selection, javaProject);
						config = wc.doSave();
					} catch (CoreException e) {
						RemoteDebugUIActivator.log(e);
						RemoteDebugUIActivator.displayLaunchError(getShell(), config, e);
					}
				}
			}
		}
		IStructuredSelection selection = new StructuredSelection(config);
		dialog.setInitialSelection(selection);
		dialog.setOpenMode(LaunchConfigurationsDialog.LAUNCH_CONFIGURATION_DIALOG_OPEN_ON_SELECTION);
		dialog.open();
	}
	
	private static Shell getShell() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window == null) {
			IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
			if (windows.length > 0) {
				return windows[0].getShell();
			}
		}
		else {
			return window.getShell();
		}
		return null;
	}
}
