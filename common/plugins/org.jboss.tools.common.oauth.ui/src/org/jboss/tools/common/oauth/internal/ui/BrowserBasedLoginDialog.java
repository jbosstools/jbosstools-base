/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.internal.ui;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.oauth.core.LoginResponse;
import org.keycloak.adapters.KeycloakDeployment;

public class BrowserBasedLoginDialog extends Dialog {

  private KeycloakDeployment deployment;
	private OAuthBrowser browser;
	private LoginResponse info;
	private Job redirectJob;

	public BrowserBasedLoginDialog(Shell parentShell, KeycloakDeployment deployment) {
		super(parentShell);
		this.deployment = deployment;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, "Close", true);
	}

	@Override
	protected boolean isResizable() {
		return true;
	}

	@Override
	protected Point getInitialSize() {
		Shell parent = getParentShell();
		return new Point(parent.getSize().x * 3 / 4, parent.getSize().y * 3 / 4);
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite container = (Composite) super.createDialogArea(parent);
		container.setLayout(new GridLayout());
		GridDataFactory.fillDefaults().align(SWT.FILL, SWT.FILL).grab(true, true).applyTo(container);

		browser = new OAuthBrowser(container, SWT.BORDER);
		browser.setText("Loading");
		Browser.clearSessions();
		GridDataFactory.fillDefaults().align(SWT.FILL, SWT.FILL).grab(true, true).applyTo(browser);

		final ProgressBar progressBar = new ProgressBar(container, SWT.NONE);
		GridDataFactory.fillDefaults().align(SWT.FILL, SWT.FILL).applyTo(progressBar);
		
		browser.getRedirectFuture().handle((response, t) -> {
		  info = response;
		  BrowserBasedLoginDialog.this.getShell().getDisplay().asyncExec(BrowserBasedLoginDialog.this::close);
		  return 0;
		});
		browser.setDeployment(deployment);
		return container;
	}

	/**
	 * @return the info
	 */
	public LoginResponse getInfo() {
		return info;
	}

}
