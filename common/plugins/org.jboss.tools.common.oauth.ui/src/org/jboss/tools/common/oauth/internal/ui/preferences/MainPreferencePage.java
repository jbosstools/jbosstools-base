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
package org.jboss.tools.common.oauth.internal.ui.preferences;

import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.common.oauth.core.AccountService;
import org.jboss.tools.common.oauth.core.model.IAccount;
import org.jboss.tools.common.oauth.core.model.IAuthorizationServer;

import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class MainPreferencePage extends org.eclipse.jface.preference.PreferencePage
		implements IWorkbenchPreferencePage {

	private boolean removed = false;

	private Label label;

	private Button button;

	public MainPreferencePage() {
		super();
	}
	
	private IAuthorizationServer getAuthorizationServer() {
	  if (AccountService.getDefault().getModel().getAuthorizationServers().isEmpty()) {
	    return null;
	  }
	  return AccountService.getDefault().getModel().getAuthorizationServers().get(0);
	}

	@Override
	public void init(IWorkbench workbench) {
	}

	@Override
	protected Control createContents(Composite parent) {
		createWidgets(parent);
		return parent;
	}

	private void createWidgets(Composite control) {
		Composite parent = new Composite(control, SWT.NONE);
		GridDataFactory.fillDefaults().grab(true, true).hint(SWT.FILL, SWT.FILL).applyTo(parent);
		GridLayoutFactory.fillDefaults().applyTo(parent);

		label = new Label(parent, SWT.WRAP);
		updateLabel();

		button = new Button(parent, SWT.PUSH);
		button.setText("Remove");
		button.addSelectionListener(SelectionListener.widgetSelectedAdapter(this::onButton));
		IAuthorizationServer server = getAuthorizationServer();
		if (server == null || server.getAccounts().isEmpty()) {
			button.setEnabled(false);
		}
	}

	void onButton(SelectionEvent event) {
		button.setEnabled(false);
		removed = true;
		updateLabel();

	}

	void updateLabel() {
		String msg;
		IAuthorizationServer server = getAuthorizationServer();
		if (removed || server == null || server.getAccounts().isEmpty()) {
			msg = "No configured accounts";
		} else {
			IAccount account = server.getAccounts().get(0);
			msg = account.getId() + " account configured valid until "
					+ Date.from(Instant.ofEpochMilli(account.getAccessTokenExpiryTime()));
		}
		label.setText(msg);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		if (removed) {
			IAuthorizationServer server = getAuthorizationServer();
			if (server != null && !server.getAccounts().isEmpty()) {
				IAccount account = server.getAccounts().get(0);
				server.removeAccount(account);
				server.save();
			}
		}
		return true;
	}

}