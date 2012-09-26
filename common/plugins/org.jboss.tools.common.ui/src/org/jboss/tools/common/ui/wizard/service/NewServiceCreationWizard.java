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

import java.io.ByteArrayInputStream;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.wizards.NewElementWizard;
import org.eclipse.jdt.ui.wizards.NewClassWizardPage;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.util.FileUtil;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class NewServiceCreationWizard extends NewElementWizard {
	protected NewTypeWizardPage fPage;

	public NewServiceCreationWizard() {
		setWindowTitle(CommonUIMessages.NEW_SERVICE_WIZARD_TITLE);
	}

	/*
	 * @see Wizard#createPages
	 */
	public void addPages() {
		super.addPages();
		if (fPage == null) {
			fPage = new  NewServiceWizardPage();
			((NewClassWizardPage)fPage).init(getSelection());
			//init page
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
		if (res) {
			IResource resource= fPage.getModifiedResource();
			if (resource != null) {
				selectAndReveal(resource);
				openResource((IFile) resource);
			}
		}
		return res;
	}

	private void registerService() throws CoreException {
		String typeName = fPage.getCreatedType().getFullyQualifiedName();
		String serviceType = ((NewServiceWizardPage)fPage).getServiceRawType();
		IContainer f = getServiceFolder();
		if(f != null) {
			IFile file = f.getFile(new Path(serviceType));
			if(file.exists()) {
				String content = FileUtil.readStream(file);
				if(content.length() > 0 && !content.endsWith("\n")) { //$NON-NLS-1$
					content += "\n"; //$NON-NLS-1$
				}
				content += typeName;
				file.setContents(new ByteArrayInputStream(content.getBytes()), true, true, new NullProgressMonitor());
			} else {
				String content = typeName;
				file.create(new ByteArrayInputStream(content.getBytes()), true, new NullProgressMonitor());
			}	
		}
	}

	static final String META_INF_FOLDER_NAME = "META-INF"; //$NON-NLS-1$
	static final String SERVICES_FOLDER_NAME = "services"; //$NON-NLS-1$

	private IFolder getServiceFolder() throws CoreException {
		IContainer m = getMetaInf();
		if(m != null) {
			IFolder ss = m.getFolder(new Path(SERVICES_FOLDER_NAME));
			if(!ss.exists()) {
				ss.create(true, true, new NullProgressMonitor());
			}
			return ss;
		}
		return null;
	}

	private IContainer getMetaInf() throws CoreException {
		IProject project = fPage.getCreatedType().getResource().getProject();
		IResource[] rs = EclipseUtil.getJavaSourceRoots(project);
		if(rs == null || rs.length == 0) {
			return null;
		}
		for (IResource r: rs) {
			IFolder f = ((IContainer)r).getFolder(new Path(META_INF_FOLDER_NAME));
			if(f.exists()) return f;
		}
		IFolder f = ((IContainer)rs[0]).getFolder(new Path(META_INF_FOLDER_NAME));
		f.create(true, true, new NullProgressMonitor());
		return f;
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
