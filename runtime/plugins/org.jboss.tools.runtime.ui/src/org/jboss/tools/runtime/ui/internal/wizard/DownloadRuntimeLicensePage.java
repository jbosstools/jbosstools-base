/*************************************************************************************
 * Copyright (c) 2010-2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/

package org.jboss.tools.runtime.ui.internal.wizard;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * 
 * @author snjeza
 *
 */
public class DownloadRuntimeLicensePage extends WizardPage {

	private static final String DOWNLOAD_RUNTIME_SECTION = "downloadRuntimeSection";
	private Button accept;
	private Button decline;
	private Browser browser;
	private DownloadRuntime downloadRuntime;
	private IDialogSettings downloadRuntimeSection;

	protected DownloadRuntimeLicensePage(DownloadRuntime downloadRuntime) {
		super("DownloadRuntimeLicensePage");
		setTitle("Runtime License");
		this.downloadRuntime = downloadRuntime;
		setDescription("This license must be accepted before proceeding with the installation.");
		IDialogSettings dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		downloadRuntimeSection = dialogSettings.getSection(DOWNLOAD_RUNTIME_SECTION);
		if (downloadRuntimeSection == null) {
			downloadRuntimeSection = dialogSettings.addNewSection(DOWNLOAD_RUNTIME_SECTION);
		}
	}

	@Override
	public void createControl(Composite parent) {

		Composite contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		
		
		try {
			browser = new Browser(contents, SWT.NONE);
		} catch (Exception e1) {
			browser = new Browser(contents, SWT.WEBKIT);
		}
		
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.heightHint = 150;
		browser.setLayoutData(gd);
		
		accept = new Button(contents, SWT.RADIO);
		accept.setText("I &accept the terms of the license agreement");
		accept.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				setPageComplete(accept.getSelection());
			}
			
		});
		
		decline = new Button(contents, SWT.RADIO);
		decline.setText("I &do not accept the terms of the license agreement");
		decline.setSelection(true);
		decline.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				setPageComplete(accept.getSelection());
			}
			
		});
		
		setDownloadRuntime(downloadRuntime);
		
		setControl(contents);
	}

	@Override
	public boolean canFlipToNextPage() {
		if (accept == null || accept.isDisposed()) {
			return false;
		}
		return super.canFlipToNextPage() && accept.getSelection();
	}

	public void setDownloadRuntime(DownloadRuntime downloadRuntime) {
		this.downloadRuntime = downloadRuntime;
		if (downloadRuntime != null) {
			if (browser != null && downloadRuntime.getLicenceURL() != null
					&& !downloadRuntime.getLicenceURL().isEmpty()) {
				browser.setText("<html></html>");
				browser.setUrl(downloadRuntime.getLicenceURL());
			}
			setTitle("Runtime '" + downloadRuntime.getName() + "'");
			boolean accepted = isAccepted(downloadRuntime);
			if (decline != null) {
				decline.setSelection(!accepted);
				accept.setSelection(accepted);
				setPageComplete(accepted);
			}
		} else if (decline != null) {
			decline.setSelection(true);
			accept.setSelection(false);
			setPageComplete(false);
		}
	}

	public boolean isAccepted(DownloadRuntime downloadRuntime) {
		if (downloadRuntime == null) {
			return false;
		}
		return downloadRuntimeSection.getBoolean(downloadRuntime.getId());
	}

	public void finishPage() {
		if (downloadRuntime != null && accept != null && !accept.isDisposed()) {
			downloadRuntimeSection.put(downloadRuntime.getId(), accept.getSelection());
		}
	}

}
