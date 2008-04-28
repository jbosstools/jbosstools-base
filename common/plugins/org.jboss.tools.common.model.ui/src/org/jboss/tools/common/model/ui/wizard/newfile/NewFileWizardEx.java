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
package org.jboss.tools.common.model.ui.wizard.newfile;

import org.jboss.tools.common.model.ui.wizards.standard.DefaultStandardWizard;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.jboss.tools.common.model.ui.ModelUIImages;

public abstract class NewFileWizardEx extends DefaultStandardWizard implements INewWizard {
	protected NewFileContextEx context;

	public NewFileWizardEx() {
		context = createNewFileContext();
		context.init();
		setSupport(context.getSupport());
		setWindowTitle(context.getWindowTitle());
		setDefaultPageImageDescriptor(ModelUIImages.getImageDescriptor(ModelUIImages.WIZARD_NEW_PROJECT));
	}

	protected abstract NewFileContextEx createNewFileContext();
	
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		context.setSelection(selection);
	}

}
