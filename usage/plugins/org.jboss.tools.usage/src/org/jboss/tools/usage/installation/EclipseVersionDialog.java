/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.installation;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferences;
import org.jboss.tools.usage.util.BrowserUtil;

/**
 * @author Alexey Kazakov
 */
public class EclipseVersionDialog extends Dialog {

	private Button doNotShowAgainButton;

	public EclipseVersionDialog(IShellProvider parentShell) {
		super(parentShell);
	}
	
	public EclipseVersionDialog(Shell parentShell) {
		super(parentShell);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		getButton(IDialogConstants.OK_ID).setFocus();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	@Override
	protected void okPressed() {
		if(doNotShowAgainButton.getSelection()) {
			UsageReportPreferences.setEclipseVerionChecking(false);
		}
		super.okPressed();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		createWidgets(parent, composite);
		applyDialogFont(composite);

		return composite;
	}

	private void createWidgets(Composite parent, Composite composite) {
		Link link = new Link(composite, SWT.WRAP);
		link.setFont(parent.getFont());

		link.setText(CompatibilityMessages.CompatibilityCheckerDialogWarningMessage);
		link.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				BrowserUtil.checkedCreateExternalBrowser(
							e.text,
							JBossToolsUsageActivator.PLUGIN_ID,
							JBossToolsUsageActivator.getDefault().getLog());
			}
		});
		GridDataFactory.fillDefaults()
					.align(SWT.FILL, SWT.CENTER)
					.grab(true, false)
					.hint(500, SWT.DEFAULT)
					.applyTo(link);

		doNotShowAgainButton = new Button(composite, SWT.CHECK);
		doNotShowAgainButton.setSelection(false);
		doNotShowAgainButton.setText(CompatibilityMessages.DoNotShowAgain);
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.horizontalSpan = 2;
		doNotShowAgainButton.setLayoutData(data);

		// create a composite with standard margins and spacing
		Composite comp = new Composite(parent, SWT.NONE);
		comp.setLayout(createLayout());
		GridData childData = new GridData(GridData.FILL_BOTH);
		childData.horizontalSpan = 2;
		comp.setLayoutData(childData);
		comp.setFont(parent.getFont());
	}

	private GridLayout createLayout() {
		GridLayout gl = new GridLayout(2, false);
		gl.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
		gl.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
		gl.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
		gl.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
		return gl;
	}
}