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
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.Messages;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * 
 * @author snjeza
 *
 */
public class DownloadRuntimeLicensePage extends WizardPage {

	private static final String DOWNLOAD_RUNTIME_SECTION = "downloadRuntimeSection"; //$NON-NLS-1$
	private Button accept;
	private Button decline;
	private Browser browser;
	private DownloadRuntime downloadRuntime;
	private IDialogSettings downloadRuntimeSection;

	protected DownloadRuntimeLicensePage(DownloadRuntime downloadRuntime) {
		super("DownloadRuntimeLicensePage"); //$NON-NLS-1$
		setTitle(Messages.DownloadRuntimeLicensePage_Runtime_License);
		this.downloadRuntime = downloadRuntime;
		setDescription(Messages.DownloadRuntimeLicensePage_This_license_must_be_accepted);
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
		
		Composite wrap = new Composite(contents, SWT.BORDER);
		wrap.setLayout(new GridLayout(1,  false));
		try {
			browser = new Browser(wrap, SWT.NONE);
		} catch (Exception e1) {
			browser = new Browser(wrap, SWT.WEBKIT);
		}
		
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.heightHint = 150;
		browser.setLayoutData(gd);
		wrap.setLayoutData(gd);
		
		accept = new Button(contents, SWT.RADIO);
		accept.setText(Messages.DownloadRuntimeLicensePage_I_accept_the_terms);
		accept.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				setPageComplete(accept.getSelection());
			}
			
		});
		
		decline = new Button(contents, SWT.RADIO);
		decline.setText(Messages.DownloadRuntimeLicensePage_I_do_not_accept_the_terms);
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
				browser.setText("<html></html>"); //$NON-NLS-1$
				browser.setUrl(downloadRuntime.getLicenceURL());
			}
			setTitle(NLS.bind(Messages.DownloadRuntimeLicensePage_Runtime, downloadRuntime.getName()));
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
		if (downloadRuntime != null && 
				(downloadRuntime.getLicenceURL() == null || downloadRuntime.getLicenceURL().isEmpty())) {
			return true;
		}
		return downloadRuntimeSection.getBoolean(downloadRuntime.getId());
	}

	public void finishPage() {
		if (downloadRuntime != null && accept != null && !accept.isDisposed()) {
			downloadRuntimeSection.put(downloadRuntime.getId(), accept.getSelection());
		}
	}

}
