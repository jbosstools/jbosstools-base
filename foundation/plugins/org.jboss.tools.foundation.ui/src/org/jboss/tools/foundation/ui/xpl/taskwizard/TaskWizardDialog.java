/*******************************************************************************
 * Copyright (c) 2014 Red Hat and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package org.jboss.tools.foundation.ui.xpl.taskwizard;

import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * An extension to your standard WizardDialog
 * that allows task wizard pages and wizard fragments
 * to run long-running tasks before switching to 
 * the next page, when "Next" is pressed. This is 
 * a common use case throughout JBossTools. 
 */
public class TaskWizardDialog extends WizardDialog {

	public TaskWizardDialog(Shell parentShell, IWizard newWizard) {
		super(parentShell, newWizard);
	}

	protected void nextPressed() {
		IWizardPage currentPage = getCurrentPage();
		if( currentPage instanceof TaskWizardPage) {
			TaskWizardPage cp = ((TaskWizardPage)currentPage);
			if( cp.hasActionOnNextPressed()) {
				boolean ret = cp.performNextPressedAction();
				if( !ret ) {
					// We should not change the page
					return;
				}
			}
		}
		super.nextPressed();
	}
}
