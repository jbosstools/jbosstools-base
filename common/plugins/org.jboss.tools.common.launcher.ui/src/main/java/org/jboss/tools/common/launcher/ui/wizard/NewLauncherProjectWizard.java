/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.ui.wizard;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.jboss.tools.common.ui.wizard.AbstractModelWizard;

public class NewLauncherProjectWizard extends AbstractModelWizard<NewLauncherProjectModel> implements INewWizard {

	private class CreateJob extends WorkspaceJob {

		public CreateJob(String name) {
			super(name);
		}

		@Override
		public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
			return new NewLauncherProjectWizardController(getModel()).run(monitor);
		}
		
	}
	/**
	 * 
	 */
	public NewLauncherProjectWizard() {
		super("New Launcher project", new NewLauncherProjectModel());
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
	}
	
	@Override
	public void addPages() {
		addPage(new NewLauncherProjectWizardPage(this, getModel()));
	}

	@Override
	public boolean performFinish() {
		new CreateJob("Create launcher project").schedule();
		return true;
	}
}
