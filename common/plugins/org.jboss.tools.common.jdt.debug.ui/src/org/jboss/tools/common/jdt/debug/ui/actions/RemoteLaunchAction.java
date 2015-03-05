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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.internal.ui.stringsubstitution.SelectedResourceManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ITypeRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.launcher.LauncherMessages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IEditorPart;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.ui.internal.SelectionUtil;

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
		IResource resource = SelectionUtil.getLaunchableResource(selection);
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
		try {
			return RemoteDebugActivator.createOrGetDefaultLaunchConfiguration(port, RemoteDebugActivator.LOCALHOST, javaProject, 
					SelectionUtil.getJavaElements(selection));
		} catch (CoreException exception) {
			MessageDialog.openError(JDIDebugUIPlugin.getActiveWorkbenchShell(),
					LauncherMessages.JavaLaunchShortcut_3, exception
							.getStatus().getMessage());
		}
		return config;
	}

}
