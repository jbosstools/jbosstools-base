/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

package org.jboss.tools.common.ui.wizard.service;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.wizards.NewElementWizard;
import org.eclipse.jdt.ui.wizards.NewClassWizardPage;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class NewServiceCreationWizard extends NewElementWizard {
	protected NewTypeWizardPage fPage;
	boolean openEditorAfterFinish = true;

	public NewServiceCreationWizard() {
		setWindowTitle(CommonUIMessages.NEW_SERVICE_WIZARD_TITLE);
	}

	public void setOpenEditorAfterFinish(boolean b) {
		openEditorAfterFinish = b;
	}

	/*
	 * @see Wizard#createPages
	 */
	public void addPages() {
		super.addPages();
		if (fPage == null) {
			fPage = new  NewServiceWizardPage();
			((NewClassWizardPage)fPage).init(getSelection());
		}
		addPage(fPage);
	}

	/*(non-Javadoc)
	 * @see org.eclipse.jdt.internal.ui.wizards.NewElementWizard#canRunForked()
	 */
	protected boolean canRunForked() {
		return !fPage.isEnclosingTypeSelected();
	}

	public boolean performFinish() {
		boolean res = super.performFinish();
		if(res && ((NewServiceWizardPage)fPage).isToBeRegisteredInMetaInf()) {
			try {
				registerService();
			} catch (CoreException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
		}
		if (res && openEditorAfterFinish) {
			IResource resource= fPage.getModifiedResource();
			if (resource != null) {
				selectAndReveal(resource);
				openResource((IFile) resource);
			}
		}
		return res;
	}

	private void registerService() throws CoreException {
		IProject project = fPage.getCreatedType().getResource().getProject();
		String typeName = fPage.getCreatedType().getFullyQualifiedName();
		String serviceType = ((NewServiceWizardPage)fPage).getServiceRawType();
		RegisterServiceUtil.registerService(project, typeName, serviceType);
	}

	@Override
	protected void finishPage(IProgressMonitor monitor) throws InterruptedException, CoreException {
		fPage.createType(monitor); // use the full progress monitor
	}

	@Override
	public IJavaElement getCreatedElement() {
		return fPage.getCreatedType();
	}
}
