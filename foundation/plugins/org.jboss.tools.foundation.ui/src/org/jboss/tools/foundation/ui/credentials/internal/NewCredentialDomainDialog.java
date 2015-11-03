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
package org.jboss.tools.foundation.ui.credentials.internal;

import java.util.Arrays;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.core.credentials.ICredentialsModel;
import org.jboss.tools.foundation.ui.util.FormDataUtility;

public class NewCredentialDomainDialog extends TitleAreaDialog {

	private ICredentialsModel model;
	private String domainName;
	
	// Used when editing a domain instead of creating a new one
	private ICredentialDomain domain;
	private String defaultUser;
	
	private Combo defaultUserCombo;
	private Text domainText;
	
	public NewCredentialDomainDialog(Shell parentShell, ICredentialsModel model) {
		super(parentShell);
		this.model = model;
	}
	
	/*
	 * Edit an existing domain
	 */
	public NewCredentialDomainDialog(Shell parentShell, ICredentialsModel model, ICredentialDomain domain) {
		super(parentShell);
		this.model = model;
		this.domain = domain;
	}

	
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		Shell s2 = shell.getParent().getShell();
		shell.setLocation(s2.getLocation());
		shell.setBounds(shell.getLocation().x, shell.getLocation().y, 550, 500);
		if( domain == null )
			shell.setText(CredentialMessages.AddACredentialDomain);
		else {
			shell.setText(CredentialMessages.EditACredentialDomain);
		}
	}
    protected int getShellStyle() {
        int ret = super.getShellStyle();
        return ret | SWT.RESIZE;
    }
	protected Control createDialogArea(Composite parent) {
		if( domain == null ) {
			setTitle(CredentialMessages.AddACredentialDomain);
		} else {
			setTitle(CredentialMessages.EditACredentialDomain);
		}
		
		Composite main = new Composite((Composite)super.createDialogArea(parent), SWT.NONE);
		main.setLayoutData(new GridData(GridData.FILL_BOTH));
		main.setLayout(new FormLayout());
		
		domainText = new Text(main, SWT.SINGLE | SWT.BORDER);
		Label l = new Label(main, SWT.NONE);
		l.setText(CredentialMessages.NewDomainNameLabel);
		
		l.setLayoutData(new FormDataUtility().createFormData(0, 4, null, 0, 0, 5, null, 0));
		domainText.setLayoutData(new FormDataUtility().createFormData(null, 0, null, 0, l, 5, 100, -5));
		
		
		if( domain != null ) {
			addDefaultUsernameCombo(main, domainText);;
		}
		
		addListeners();
		return main;
	}
	
	private void addListeners() {
		
		domainText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				domainName = domainText.getText();
				ICredentialDomain[] domains = model.getDomains();
				for( int i = 0; i < domains.length; i++ ) {
					if( domains[i].getName().equals(domainText.getText())) {
						setMessage(CredentialMessages.DomainNameExists, IMessageProvider.ERROR);
						getButton(IDialogConstants.OK_ID).setEnabled(false);
						return;
					}
					if( domains[i].getId().equals(domainText.getText())) {
						setMessage(CredentialMessages.DomainIdExists, IMessageProvider.ERROR);
						getButton(IDialogConstants.OK_ID).setEnabled(false);
						return;
					}
					setMessage(null, IMessageProvider.NONE);
					getButton(IDialogConstants.OK_ID).setEnabled(true);
				}
			}
		});
		
		if( defaultUserCombo != null ) {
			defaultUserCombo.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					int sel = defaultUserCombo.getSelectionIndex();
					if( sel != -1 ) {
						defaultUser = defaultUserCombo.getItem(sel);
					}
				}
			});
		}
	}
	
	private void addDefaultUsernameCombo(Composite main, Text domainText) {
		// We're editing an existing domain
		domainText.setText(domain.getName());
		String[] users = domain.getUsernames();
		if( users.length > 0 ) {
			domainText.setEnabled(false);
			defaultUser = domain.getDefaultUsername();
			// We should show a combo here to select the default user
			Label comboLabel = new Label(main, SWT.NONE);
			comboLabel.setText("Default user: ");
			comboLabel.setLayoutData(new FormDataUtility().createFormData(domainText, 8, null, 0, 0, 5, null, 0));

			defaultUserCombo = new Combo(main, SWT.READ_ONLY);
			defaultUserCombo.setItems(domain.getUsernames());
			defaultUserCombo.setLayoutData(new FormDataUtility().createFormData(domainText, 5, null, 0, comboLabel, 5, 100, -5));
			
			int toSelect = defaultUser == null ? -1 : Arrays.asList(users).indexOf(defaultUser);
			if( toSelect != -1 ) {
				defaultUserCombo.select(toSelect);
			}
		}
	}
	
	public String getDefaultUser() {
		return defaultUser;
	}
	
	public String getDomainName() {
		return domainName;
	}
}
