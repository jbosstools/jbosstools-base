/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Andre Dietisheim
 */
public class UsageReportEnablementDialog extends Dialog {

	private Button checkBox;
	private String title;
	private String message;
	private boolean reportEnabled;
	private String checkBoxLabel;

	public UsageReportEnablementDialog(String title, String message, String checkBoxLabel,
			boolean reportEnabled, IShellProvider parentShell) {
		super(parentShell);
		this.title = title;
		this.message = message;
		this.checkBoxLabel = checkBoxLabel;
		this.reportEnabled = reportEnabled;
	}

	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			this.reportEnabled = checkBox.getSelection();
		}
		else if (buttonId == IDialogConstants.CANCEL_ID) {
			this.reportEnabled = false;
		}
		super.buttonPressed(buttonId);
	}

	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		if (title != null) {
			shell.setText(title);
		}
	}

	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
		checkBox.setFocus();
		checkBox.setSelection(reportEnabled);
	}

	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		if (message != null) {
			Label label = new Label(composite, SWT.WRAP);
			label.setFont(parent.getFont());
			label.setText(message);
			GridDataFactory.fillDefaults()
				.align(SWT.FILL, SWT.CENTER).grab(true, false)
				.hint(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH, SWT.DEFAULT)
				.applyTo(label);
		}
		checkBox = new Button(composite, SWT.CHECK);
		checkBox.setText(checkBoxLabel);
		GridDataFactory.fillDefaults().grab(true, false).align(SWT.LEFT, SWT.CENTER).applyTo(checkBox);
		applyDialogFont(composite);
		return composite;
	}

	public boolean isReportEnabled() {
		return reportEnabled;
	}
}
