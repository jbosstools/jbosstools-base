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
package org.jboss.tools.common.model.ui.editors.dnd;

import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class DropWizardDialog extends WizardDialog {
	private static final int DO_WIDTH_VALUE = 450;
	private static final int DO_HEIGHT_VALUE = 530;
	private static final int DO_WIDTH_VALUE_LINUX = 550;

	public DropWizardDialog(Shell parentShell, IWizard newWizard) {
		super(parentShell, newWizard);
	}
	
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setBounds(
			ModelPlugin.getDefault().getWorkbench().getDisplay().getBounds().width/2-DO_WIDTH_VALUE/2,
			ModelPlugin.getDefault().getWorkbench().getDisplay().getBounds().height/2-DO_HEIGHT_VALUE/2,				
			getPreferredWidth(),
			DO_HEIGHT_VALUE
		);
	}
	
	protected int getPreferredWidth() {
		try {
			String os_name = System.getProperty("os.name");
			if(os_name != null && os_name.indexOf("Windows") >= 0) return DO_WIDTH_VALUE;
		} catch (Exception e) {
			//ignore
		}
		return DO_WIDTH_VALUE_LINUX;
	}
}
