/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.test;

import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.model.ui.wizards.standard.DefaultStandardStep;
import org.jboss.tools.common.util.WorkbenchUtils;

import junit.framework.TestCase;


/**
 * @author eskimo
 *
 */
public class ModelNewWizardTest extends TestCase {
	protected void testNewWizardInstanceIsCreated(String id) {
		IWizard
		aWizard = WorkbenchUtils.findWizardByDefId(
				id);
		
		WizardDialog dialog = new WizardDialog(
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
				aWizard);
		dialog.setBlockOnOpen(false);
		try {
			dialog.open();
			IWizardPage page = dialog.getCurrentPage();
			assertTrue("Start page is not loaded",page instanceof DefaultStandardStep);
		} finally {
			dialog.close();
		}
	}
}
