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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * 
 * @author snjeza
 *
 */
public class DownloadRuntimeLicenseFragment extends WizardFragment {

	private static final String DOWNLOAD_RUNTIME_SECTION = "downloadRuntimeSection"; //$NON-NLS-1$
	private Button accept;
	private Button decline;
	private Browser browser;
	private DownloadRuntime dlrt;
	private IDialogSettings downloadRuntimeSection;
	private IWizardHandle handle;

	public DownloadRuntimeLicenseFragment() {
		IDialogSettings dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		downloadRuntimeSection = dialogSettings.getSection(DOWNLOAD_RUNTIME_SECTION);
		if (downloadRuntimeSection == null) {
			downloadRuntimeSection = dialogSettings.addNewSection(DOWNLOAD_RUNTIME_SECTION);
		}
	}

	@Override
	public boolean hasComposite() {
		return true;
	}

	@Override
	protected void createChildFragments(List<WizardFragment> list) {
		// TODO
	}

	@Override
	public void enter() {
		DownloadRuntime tmp = getDownloadRuntimeFromTaskModel();
		if (tmp != null && !tmp.equals(dlrt)) {
			dlrt = tmp;
			setDownloadRuntime(dlrt);
		}
	}

	@Override
	public Composite createComposite(Composite parent, IWizardHandle handle) {
		this.handle = handle;
		getPage().setTitle(Messages.DownloadRuntimeLicensePage_Runtime_License);
		getPage().setDescription(Messages.DownloadRuntimeLicensePage_This_license_must_be_accepted);

		Composite contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		
		Composite wrap = new Composite(contents, SWT.BORDER);
		wrap.setLayout(new GridLayout(1,  false));
		
		
		
		browser = createBrowser(SWT.NONE, wrap, SWT.WEBKIT);
		
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.heightHint = 150;
		browser.setLayoutData(gd);
		wrap.setLayoutData(gd);
		
		accept = new Button(contents, SWT.RADIO);
		accept.setText(Messages.DownloadRuntimeLicensePage_I_accept_the_terms);
		accept.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				setComplete(accept.getSelection());
				DownloadRuntimeLicenseFragment.this.handle.update();
			}
			
		});
		
		decline = new Button(contents, SWT.RADIO);
		decline.setText(Messages.DownloadRuntimeLicensePage_I_do_not_accept_the_terms);
		decline.setSelection(true);
		decline.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				setComplete(accept.getSelection());
				DownloadRuntimeLicenseFragment.this.handle.update();
			}
			
		});
		
		setDownloadRuntime(getDownloadRuntimeFromTaskModel());
		return contents;
	}

	private static boolean browserLoadingErrorLoged = false;
	private static Map<Integer, String> browserNames = new HashMap<Integer, String>();
	static {
		browserNames.put(SWT.WEBKIT, "Webkit"); //$NON-NLS-1$
		browserNames.put(SWT.MOZILLA, "Mozilla"); //$NON-NLS-1$
	}

	private static Browser createBrowser(int style, Composite parent, int preferredBrowser) {
		try {
			return new Browser(parent, style | preferredBrowser);
		} catch (Error e) {
			if (!(e instanceof SWTError) && !browserLoadingErrorLoged) {
				logBrowserLoadingProblem(e, browserNames.get(preferredBrowser), true);
			}
			Control[] children = parent.getChildren();
			for (Control child : children) {
				child.dispose();
			}
			try {
				return new Browser(parent, style | SWT.NONE);
			} catch (Error e1) {
				logBrowserLoadingProblem(e1, null, false);
			}
		}
		return null;

	}

	private static void logBrowserLoadingProblem(Error e, String browserName, boolean warning) {
		if (browserName == null) {
			browserName = "default";
		}
		String message = "Cannot create " + browserName + " browser";
		Exception ex = new Exception(message, e);
		if (warning) {
			RuntimeUIActivator.pluginLog().logWarning(ex);
		} else {
			RuntimeUIActivator.pluginLog().logError(ex);
		}
		browserLoadingErrorLoged = true;
	}

	private void setDownloadRuntime(DownloadRuntime downloadRuntime) {
		if (downloadRuntime != null) {
			if (browser != null && downloadRuntime.getLicenceURL() != null
					&& !downloadRuntime.getLicenceURL().isEmpty()) {
				browser.setText("<html></html>"); //$NON-NLS-1$
				browser.setUrl(downloadRuntime.getLicenceURL());
			}
			getPage().setTitle(NLS.bind(Messages.DownloadRuntimeLicensePage_Runtime, downloadRuntime.getName()));
			boolean accepted = isAccepted(downloadRuntime);
			if (decline != null) {
				decline.setSelection(!accepted);
				accept.setSelection(accepted);
				setComplete(accepted);
			}
		} else if (decline != null) {
			decline.setSelection(true);
			accept.setSelection(false);
			setComplete(false);
		}
		handle.update();
	}

	public boolean isAccepted(DownloadRuntime downloadRuntime) {
		if (downloadRuntime == null) {
			return false;
		}
		if (downloadRuntime != null
				&& (downloadRuntime.getLicenceURL() == null || downloadRuntime.getLicenceURL().isEmpty())) {
			return true;
		}
		return downloadRuntimeSection.getBoolean(downloadRuntime.getId());
	}

	public void finishPage() {
		DownloadRuntime dl = getDownloadRuntimeFromTaskModel();
		if (dl != null && accept != null && !accept.isDisposed()) {
			downloadRuntimeSection.put(dl.getId(), accept.getSelection());
		}
	}

	private DownloadRuntime getDownloadRuntimeFromTaskModel() {
		return (DownloadRuntime) getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP);
	}
}
