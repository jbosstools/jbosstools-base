/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.credentials;

import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.foundation.core.credentials.CredentialService;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;


public class ChooseCredentialOverridePasswordComponent extends ChooseCredentialComponent implements ICredentialCompositeListener {
	private Text passwordText;
	private Label passwordLabel;
	private ModifyListener passwordModifyListener;
	private boolean modifyingPassword;
	private boolean passwordModified = false;
	
	/**
	 * Draw the credential selection composite allowing all domains
	 */
	public ChooseCredentialOverridePasswordComponent() {
		this(null);
	}
	
	/**
	 * Draw the credential selection composite allowing selected domains, 
	 * or, if null, all domains. 
	 * 
	 * @param parent	The parent composite
	 * @param domains	The selected domains, or null to mean all domains
	 */
	public ChooseCredentialOverridePasswordComponent(String[] domains) {
		this(domains, null);
	}
	
	/**
	 * 
	 * @param parent
	 * @param domains
	 * @param selectedUsername
	 */
	public ChooseCredentialOverridePasswordComponent(String[] domains, String selectedUsername) {
		super(domains, selectedUsername);
		addCredentialListener(this);
		
	}
	
	@Override
	public void create(Composite parent) {
		super.create(parent);
		credentialsChanged();
	}

	
	protected void createWidgets(Composite parent) {
		super.createWidgets(parent);
		passwordLabel = new Label(parent, SWT.None);
		passwordLabel.setText("Password: ");
		passwordText = new Text(parent, SWT.BORDER | SWT.PASSWORD);
	}
	
	protected void addWidgetListeners() {
		super.addWidgetListeners();
		passwordModifyListener = 
		new ModifyListener(){
			public void modifyText(ModifyEvent e) {
				modifyingPassword = true;
				passwordModified = true;
				try {
					fireChanged();
				} finally {
					modifyingPassword = false;
				}
			}};
		passwordText.addModifyListener(passwordModifyListener);
	}
	
	public String getPassword() {
		// If the user is a prompt-every-time username, pull it from the password text
		ICredentialDomain cd = getDomain();
		if( passwordModified || CredentialService.getCredentialModel().credentialRequiresPrompt(cd, getUser())) {
			return passwordText.getText();
		}
		
		// Otherwise, pull from model
		if( cd != null ) {
			try {
				return cd.getPassword(getUser());
			} catch(StorageException se) {
				FoundationUIPlugin.pluginLog().logError(se);
			}
		}
		return null;
	}
	
	public void gridLayout(int n) {
		super.gridLayout(n);
		if( n == 2 ) {
			// We're in a 2 column grid
			GridData gd2 = new GridData();
			gd2.widthHint = 200;
			passwordText.setLayoutData(gd2);
		} else if( n >= 3 ) {
			// We're in a three column grid
			GridData passwordData = new GridData();
			passwordData.widthHint = 200;
			passwordData.horizontalSpan = n-1;
			passwordData.horizontalAlignment = SWT.FILL;
			passwordText.setLayoutData(passwordData);
		}
	}
	
	protected boolean showEditButton() {
		return false;
	}

	@Override
	public void credentialsChanged() {
		if( modifyingPassword )
			return;
		
		String user = getUser();
		ICredentialDomain cd = getDomain();
		if( user != null && cd != null ) {
			boolean requiresPrompt = CredentialService.getCredentialModel().credentialRequiresPrompt(cd, user);
			passwordText.removeModifyListener(passwordModifyListener);
			try {
				if( requiresPrompt) {
					passwordText.setText("");
				} else {
					// Null safety
					String pw = cd.getPassword(user);
					passwordText.setText(pw == null ? "" : pw);
				}
			} catch(StorageException se) {
				// ignore
			} finally {
				passwordText.addModifyListener(passwordModifyListener);
			}
		}
	}
	
}
