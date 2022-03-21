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

import java.util.List;

import org.eclipse.jface.dialogs.DialogSettings;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.jboss.tools.foundation.core.IURLProvider;
import org.jboss.tools.foundation.ui.util.BrowserUtility;
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
public class DownloadRuntimeLicenseFragment extends WizardFragment implements IURLProvider {

	private static final String DOWNLOAD_RUNTIME_SECTION = "downloadRuntimeSection"; //$NON-NLS-1$
	private Link licenseLink;
	private Button accept;
	private Button decline;
	private Browser browser;
	private DownloadRuntime dlrt;
	private IDialogSettings downloadRuntimeSection;
	private IWizardHandle handle;
	private Composite wrap;
    Clipboard cb;

	
	public DownloadRuntimeLicenseFragment() {
		downloadRuntimeSection = DialogSettings.getOrCreateSection(RuntimeUIActivator.getDefault()
						.getDialogSettings(), DOWNLOAD_RUNTIME_SECTION);
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
		if( tmp != null && !tmp.equals(dlrt)) {
			dlrt = tmp;
			setDownloadRuntime(dlrt);
		}
	}
	
	@Override
	public Composite createComposite(Composite parent, IWizardHandle handle) {
		this.cb = new Clipboard(parent.getDisplay()); 
		this.handle = handle;
		getPage().setTitle(Messages.DownloadRuntimeLicensePage_Runtime_License);
		getPage().setDescription(Messages.DownloadRuntimeLicensePage_This_license_must_be_accepted);

		Composite contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		
		wrap = new Composite(contents, SWT.BORDER);
		wrap.setLayout(new GridLayout(1,  false));
		
		browser = null;
		Control ctrl = new BrowserUtility().createBrowserOrLink(SWT.READ_ONLY, wrap, BrowserUtility.getPreferredBrowser(),
				this, 
				Messages.DownloadRuntimeLicenseFragment_Please_read_and_accept_the_license_agreement);
		
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.heightHint = 150;
		gd.widthHint = 300;
		if(ctrl instanceof Link) {
			Menu popupMenu = new Menu(ctrl);
		    MenuItem refreshItem = new MenuItem(popupMenu, SWT.NONE);
		    refreshItem.setText(Messages.DownloadRuntimeLicenseFragment_Open_in_external_browser);
		    CopyToClipboardListener copyListener = new CopyToClipboardListener();
		    refreshItem.addSelectionListener(copyListener);
		    MenuItem copyToClipboard = new MenuItem(popupMenu, SWT.NONE);
		    copyToClipboard.setText(Messages.DownloadRuntimeLicenseFragment_Copy_URL_to_clipboard);
		    copyToClipboard.addSelectionListener(new CopyToClipboardListener());
		    ctrl.setMenu(popupMenu);
		} else if( ctrl instanceof Browser ){
			browser = (Browser)ctrl;
		}
		ctrl.setLayoutData(gd);
		wrap.setLayoutData(gd);
		
		licenseLink = new Link(contents, SWT.NONE);
		GridData lgd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
		lgd.widthHint = 300;
		licenseLink.setLayoutData(lgd);;

		accept = new Button(contents, SWT.RADIO);
		accept.setText(Messages.DownloadRuntimeLicensePage_I_accept_the_terms);
		SelectionListener acceptListener = new AcceptSelectionListener();
		accept.addSelectionListener(acceptListener);
		decline = new Button(contents, SWT.RADIO);
		decline.setText(Messages.DownloadRuntimeLicensePage_I_do_not_accept_the_terms);
		decline.setSelection(true);
		decline.addSelectionListener(acceptListener);
		
		setDownloadRuntime(getDownloadRuntimeFromTaskModel());
		return contents;
	}

	private void setDownloadRuntime(final DownloadRuntime downloadRuntime) {
		if (downloadRuntime != null) {
			if (browser != null && downloadRuntime.getLicenceURL() != null
					&& !downloadRuntime.getLicenceURL().isEmpty()) {
				browser.setText("<html></html>"); //$NON-NLS-1$
				browser.setUrl(downloadRuntime.getLicenceURL());
			} else if(browser == null && downloadRuntime.getLicenceURL() != null
					&& !downloadRuntime.getLicenceURL().isEmpty() ) {
				
			}
			getPage().setTitle(NLS.bind(Messages.DownloadRuntimeLicensePage_Runtime, downloadRuntime.getName()));
			String licUrl = downloadRuntime.getLicenceURL();
			String txt = "License URL: <a href=\"" + licUrl + "\">" + licUrl + "</a>";
			licenseLink.setText(txt);
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
		if (downloadRuntime != null && 
				(downloadRuntime.getLicenceURL() == null || downloadRuntime.getLicenceURL().isEmpty())) {
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
		return (DownloadRuntime)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP);
	}

	private final class AcceptSelectionListener extends SelectionAdapter {
		@Override
		public void widgetSelected(SelectionEvent e) {
			setComplete(accept.getSelection());
			DownloadRuntimeLicenseFragment.this.handle.update();
		}
	}

	private final class CopyToClipboardListener extends SelectionAdapter {
		@Override
		public void widgetSelected(SelectionEvent e) {
		    TextTransfer textTransfer = TextTransfer.getInstance();
		    cb.setContents(new Object[] { getDownloadRuntimeFromTaskModel().getLicenceURL() },
		        new Transfer[] { textTransfer });
		}
	}

	@Override
	public String getUrl() {
		return dlrt.getLicenceURL();
	}
}
