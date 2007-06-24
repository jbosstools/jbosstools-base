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

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.*;
import org.eclipse.ui.*;

import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.ModelUIImages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class NewFileWizard extends Wizard implements INewWizard {
	protected NewFileContext context;
	protected NewFileWizardPage page;
	
	public NewFileWizard() {
		context = createNewFileContext();
		context.init();
		setWindowTitle(context.getWindowTitle());
		setDefaultPageImageDescriptor(ModelUIImages.getImageDescriptor(ModelUIImages.WIZARD_NEW_PROJECT));
	}
	
	protected NewFileContext createNewFileContext() {
		return new NewFileContext();
	}
	
	public void addPages() {
		page = new NewFileWizardPage(context);
		addPage(page);
	}
	
	public boolean performFinish() {
		try {
			context.execute();
		} catch (Exception e) {
			ProblemReportingHelper.reportProblem(ModelUIPlugin.PLUGIN_ID, e);
			return false;
		}
		return true;
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		context.setSelection(selection);
	}

}
