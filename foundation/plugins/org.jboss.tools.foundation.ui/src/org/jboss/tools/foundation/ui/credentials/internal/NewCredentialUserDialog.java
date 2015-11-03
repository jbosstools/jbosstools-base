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
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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

public class NewCredentialUserDialog extends TitleAreaDialog {

	private ICredentialsModel model;
	private ICredentialDomain selectedDomain;
	private String user, pass;
	private String[] domainNames;
	private ICredentialDomain[] allDomains;
	private boolean freezeUser = false;
	private boolean freezeDomain = false;
	
	
	/**
	 * Open a new user dialog.  The selected domain will be pre-selected, but not frozen.
	 * 
	 * @param parentShell
	 * @param model
	 * @param selected
	 */
	public NewCredentialUserDialog(Shell parentShell, ICredentialsModel model, ICredentialDomain selected) {
		super(parentShell);
		this.model = model;
		this.selectedDomain = selected;
		if( selected != null ) 
			freezeDomain = true;
	}
	
	/**
	 * Open a new user dialog.  The selected domain and username will be frozen. 
	 * 
	 * @param parentShell
	 * @param model
	 * @param selected
	 * @param user
	 */
	public NewCredentialUserDialog(Shell parentShell, ICredentialsModel model, ICredentialDomain selected, String user) {
		super(parentShell);
		this.model = model;
		this.selectedDomain = selected;
		this.user = user;
		freezeDomain = true;
		freezeUser = true;
	}

	
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setBounds(shell.getLocation().x, shell.getLocation().y, 550, 500);
		if( freezeUser) 
			shell.setText(CredentialMessages.EditACredentialLabel);
		else
			shell.setText(CredentialMessages.AddACredentialLabel);
	}
    protected int getShellStyle() {
        int ret = super.getShellStyle();
        return ret | SWT.RESIZE;
    }
	protected Control createDialogArea(Composite parent) {
		if( freezeUser) 
			setTitle(CredentialMessages.EditACredentialLabel);
		else
			setTitle(CredentialMessages.AddACredentialLabel);
		Composite main = new Composite((Composite)super.createDialogArea(parent), SWT.NONE);
		main.setLayoutData(new GridData(GridData.FILL_BOTH));
		main.setLayout(new FormLayout());
		
		final Combo domains = new Combo(main, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
		Label l = new Label(main, SWT.NONE);
		l.setText(CredentialMessages.DomainLabel);
		
		allDomains = model.getDomains();
		domainNames = new String[allDomains.length];
		for( int i = 0; i < allDomains.length; i++ ) {
			domainNames[i] = allDomains[i].getName();
		}
		domains.setItems(domainNames);
		
		if( selectedDomain != null ) {
			int sIndex = Arrays.asList(allDomains).indexOf(selectedDomain);
			if( sIndex != -1) {
				domains.select(sIndex);
			}
		}
		
		
		Label nameLabel = new Label(main, SWT.None);
		nameLabel.setText(CredentialMessages.UsernameLabel);
		final Text nameText = new Text(main, SWT.SINGLE | SWT.BORDER);
		Label passLabel = new Label(main, SWT.None);
		passLabel.setText(CredentialMessages.PasswordLabel);
		final Text passText = new Text(main, SWT.SINGLE | SWT.BORDER | SWT.PASSWORD);
		
		if( user != null ) {
			nameText.setText(user);
		}
		
		
		
		nameText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				user = nameText.getText();
				validate();
			}
		});
		passText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				pass = passText.getText();
				validate();
			}
		});
		
		l.setLayoutData(		new FormDataUtility().createFormData(0, 		12, null, 0, 0, 10, null, 0));
		nameLabel.setLayoutData(new FormDataUtility().createFormData(l, 		19, null, 0, 0, 10, null, 0));
		passLabel.setLayoutData(new FormDataUtility().createFormData(nameLabel, 19, null, 0, 0, 10, null, 0));
		
		domains.setLayoutData(new FormDataUtility().createFormData(0, 8, null, 0, 25, 0, 100, -5));
		nameText.setLayoutData(new FormDataUtility().createFormData(l, 15, null, 0, 25, 0, 100, -5));
		passText.setLayoutData(new FormDataUtility().createFormData(nameLabel, 15, null, 0, 25, 0, 100, -5));
		
		domains.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				int i = domains.getSelectionIndex();
				if( i != -1 ) {
					selectedDomain = allDomains[i];
				}
				validate();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		
		if( freezeDomain) {
			domains.setEnabled(false);
		}
		if( freezeUser ) {
			nameText.setEnabled(false);
		}
		
		return main;
	}
	
	private void validate() {
		if( selectedDomain == null ) {
			setMessage(CredentialMessages.SelectDomain, IMessageProvider.ERROR);
			getButton(IDialogConstants.OK_ID).setEnabled(false);
			return;
		}
		if( user == null || user.isEmpty()) {
			setMessage(CredentialMessages.UsernameCannotBeBlank, IMessageProvider.ERROR);
			getButton(IDialogConstants.OK_ID).setEnabled(false);
			return;
		}

		String[] names =  selectedDomain.getUsernames();
		if( !freezeUser && Arrays.asList(names).contains(user)) {
			setMessage(NLS.bind(CredentialMessages.UsernameAlreadyExists, user, selectedDomain.getName()), IMessageProvider.ERROR);
			getButton(IDialogConstants.OK_ID).setEnabled(false);
			return;
		}
		if( pass == null || pass.isEmpty()) {
			setMessage(CredentialMessages.PasswordCannotBeBlank, IMessageProvider.ERROR);
			getButton(IDialogConstants.OK_ID).setEnabled(false);
			return;
		}
		
		setMessage(null, IMessageProvider.NONE);
		getButton(IDialogConstants.OK_ID).setEnabled(true);
	}
	
	public ICredentialDomain getDomain() {
		return selectedDomain;
	}
	public String getUser() {
		return user;
	}
	public String getPass() {
		return pass;
	}
}