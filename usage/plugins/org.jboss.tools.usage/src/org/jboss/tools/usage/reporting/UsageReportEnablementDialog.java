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
package org.jboss.tools.usage.reporting;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.usage.internal.JBDSUtils;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.BrowserUtil;

/**
 * @author Andre Dietisheim
 */
public class UsageReportEnablementDialog extends Dialog {

	private Button checkBox;
	private boolean reportEnabled;

	public UsageReportEnablementDialog(boolean reportEnabled, IShellProvider parentShell) {
		super(parentShell);
		this.reportEnabled = reportEnabled;
	}

	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			this.reportEnabled = checkBox.getSelection();
		} else if (buttonId == IDialogConstants.CANCEL_ID) {
			this.reportEnabled = false;
		}
		super.buttonPressed(buttonId);
	}

	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(getDialogTitle());
	}

	private String getDialogTitle() {
		if (JBDSUtils.isJBDS()) {
			return ReportingMessages.UsageReport_DialogTitle_JBDS;
		} else {
			return ReportingMessages.UsageReport_DialogTitle;
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

		// message
		Link link = new Link(composite, SWT.WRAP);
		link.setFont(parent.getFont());
		link.setText(
				getLinkText());
		link.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				BrowserUtil.checkedCreateExternalBrowser(
						getExplanationUrl(),
						JBossToolsUsageActivator.PLUGIN_ID,
						JBossToolsUsageActivator.getDefault().getLog());
			}
		});
		GridDataFactory.fillDefaults()
					.align(SWT.FILL, SWT.CENTER)
					.grab(true, false)
					.hint(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH, SWT.DEFAULT)
					.applyTo(link);

		// checkbox
		checkBox = new Button(composite, SWT.CHECK);
		checkBox.setText(getCheckBoxText());
		GridDataFactory.fillDefaults().grab(true, false).align(SWT.LEFT, SWT.CENTER).applyTo(checkBox);

		applyDialogFont(composite);

		return composite;
	}

	public boolean isReportEnabled() {
		return reportEnabled;
	}
	
	private String getCheckBoxText() {
		if (JBDSUtils.isJBDS()) {
			return ReportingMessages.UsageReport_Checkbox_Text_JBDS;
		} else {
			return ReportingMessages.UsageReport_Checkbox_Text;
		}
	}

	private String getLinkText() {
		if (JBDSUtils.isJBDS()) {
			return ReportingMessages.UsageReport_DialogMessage_JBDS;
		} else {
			return ReportingMessages.UsageReport_DialogMessage;
		}
	}

	private String getExplanationUrl() {
		if (JBDSUtils.isJBDS()) {
			return ReportingMessages.UsageReport_ExplanationPage_JBDS;
		} else {
			return ReportingMessages.UsageReport_ExplanationPage;
		}
	}

}
