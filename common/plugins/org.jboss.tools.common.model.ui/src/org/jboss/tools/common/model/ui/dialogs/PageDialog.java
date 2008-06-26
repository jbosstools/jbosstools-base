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
package org.jboss.tools.common.model.ui.dialogs;

import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

public class PageDialog extends TitleAreaDialog {

	private IDialogPage dialogPage;
	private String windowTitle;
	
//	private PageDialog(Shell parentShell) {
//		super(parentShell);
//	}

	public PageDialog(Shell parentShell, IDialogPage dialogPage) {
		super(parentShell);
		this.dialogPage = dialogPage;
	}
	
	protected Control createContents(Composite parent) {
		Control control = super.createContents(parent);
		if (dialogPage!=null) {
			this.setTitle(dialogPage.getTitle());
			this.setMessage(dialogPage.getMessage());
			this.setTitle(dialogPage.getTitle());
		}
		
		updateWindowTitle();
		
		return control;
	}
	
	
	protected Control createDialogArea(Composite parent) {
		// create the top level composite for the dialog area
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.verticalSpacing = 0;
		layout.horizontalSpacing = 0;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));
		composite.setFont(parent.getFont());

		// Build the separator line
		Label titleBarSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		titleBarSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		if (dialogPage!=null) {
			dialogPage.createControl(composite);
			Control control = dialogPage.getControl();
			control.setLayoutData(new GridData(GridData.FILL_BOTH));
		}

		// Build the separator line
		Label buttonBarSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		buttonBarSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 225;
		gd.widthHint = 300;
		composite.setLayoutData(gd);

		return composite;
	}
	
	public void setWindowTitle(String newTitle) {
		windowTitle = newTitle;
		updateWindowTitle();
	}
	
	public String getWindowTitle() {
		return windowTitle;
	}
	
	private void updateWindowTitle() {
		if (getShell() == null)	return;
		if (windowTitle == null) windowTitle = "";
		getShell().setText(windowTitle);
	}
}
