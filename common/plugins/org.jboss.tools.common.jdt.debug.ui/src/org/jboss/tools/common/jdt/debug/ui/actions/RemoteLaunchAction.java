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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.internal.ui.stringsubstitution.SelectedResourceManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ITypeRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.launcher.LauncherMessages;
import org.eclipse.jdt.internal.launching.JavaMigrationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IEditorPart;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteLaunchAction extends Action {

	private String port;

	public RemoteLaunchAction(String port) {
		this.port = port;
	}

	/**
	 * Runs with either the active editor or workbench selection.
	 * 
	 * @see IAction#run()
	 */
	public void run() {
		IStructuredSelection ss = SelectedResourceManager.getDefault().getCurrentSelection();
		Object o = ss.getFirstElement();
		if(o instanceof IEditorPart) {
			launch((IEditorPart) o);
		}
		else {
			launch(ss);
		}
	}
	
	private void launch(ISelection selection) {
		IResource resource = getLaunchableResource(selection);
		IJavaProject javaProject = null;
		if (resource != null) {
			IProject project = resource.getProject();
			javaProject = JavaCore.create(project);
		}
		ILaunchConfiguration config = createOrGetDefaultLaunchConfiguration(javaProject, selection);
		DebugUITools.launch(config, ILaunchManager.DEBUG_MODE);
	}

	private void launch(IEditorPart editor) {
		IResource resource = getLaunchableResource(editor);
		IJavaProject javaProject = null;
		if (resource != null) {
			IProject project = resource.getProject();
			javaProject = JavaCore.create(project);
		}
		ILaunchConfiguration config = createOrGetDefaultLaunchConfiguration(javaProject, null);
		DebugUITools.launch(config, ILaunchManager.DEBUG_MODE);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
	 */
	public void runWithEvent(Event event) {
		run();
	}
	
	private IResource getLaunchableResource(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;
			if (ss.size() == 1) {
				Object selected = ss.getFirstElement();
				if (!(selected instanceof IJavaElement)
						&& selected instanceof IAdaptable) {
					selected = ((IAdaptable) selected)
							.getAdapter(IJavaElement.class);
				}
				if (selected instanceof IJavaElement) {
					return ((IJavaElement) selected).getResource();
				}
			}
		}
		return null;
	}

	private IResource getLaunchableResource(IEditorPart editorpart) {
		ITypeRoot element = JavaUI.getEditorInputTypeRoot(editorpart
				.getEditorInput());
		if (element != null) {
			try {
				return element.getCorrespondingResource();
			} catch (JavaModelException e) {
			}
		}
		return null;
	}
	
	protected ILaunchConfiguration createOrGetDefaultLaunchConfiguration(IJavaProject javaProject, ISelection selection) {
		ILaunchConfiguration config = RemoteDebugActivator.getDefault().getDefaultLaunchConfiguration();
		ILaunchConfigurationWorkingCopy wc = null;
		try {
			if (config != null) {
				wc = config.getWorkingCopy();
				setAttribute(wc);
				wc.doSave();
			} else {
				ILaunchConfigurationType configType = getConfigurationType();
				wc = RemoteDebugActivator.createNewLaunchConfiguration(configType);
			}
			if (javaProject != null && javaProject.isOpen()) {
				wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, javaProject.getElementName());

				try {
					JavaMigrationDelegate.updateResourceMapping(wc);
				} catch (CoreException ce) {
					RemoteDebugActivator.log(ce);
				}
			}
			setAttribute(wc);
			if (selection != null) {
				RemoteDebugUIActivator.getDefault().addSelectedProjects(wc, selection, javaProject);
			}
			config = wc.doSave();
		} catch (CoreException exception) {
			MessageDialog.openError(JDIDebugUIPlugin.getActiveWorkbenchShell(),
					LauncherMessages.JavaLaunchShortcut_3, exception
							.getStatus().getMessage());
		}
		return config;
	}

	private void setAttribute(ILaunchConfigurationWorkingCopy wc) {
		Map attrMap = new HashMap(2);
		attrMap.put("hostname", RemoteDebugActivator.LOCALHOST); //$NON-NLS-1$
		if (port != null) {
			attrMap.put("port", port); //$NON-NLS-1$
		} else {
			attrMap.put("port", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_CONNECT_MAP,
				attrMap);
	}

	private ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}

	private ILaunchConfigurationType getConfigurationType() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager
				.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
		return type;
	}

}
