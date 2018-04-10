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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.jboss.tools.foundation.core.credentials.CredentialService;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.core.credentials.ICredentialListener;
import org.jboss.tools.foundation.ui.credentials.internal.NewCredentialUserDialog;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;

public class ChooseCredentialComponent implements ICredentialListener {
	protected ArrayList<ICredentialDomain> domainList;
	protected ArrayList<ICredentialCompositeListener> listeners = new ArrayList<ICredentialCompositeListener>();
	protected String initialUsername;
	protected SelectionListener userComboListener, domainComboListener;
	
	// Widgets
	protected Combo domainCombo, userCombo;
	protected Label domainLabel, usernameLabel;
	protected Button addUser, editUser;
	
	
	/**
	 * Draw the credential selection composite allowing all domains
	 */
	public ChooseCredentialComponent() {
		this(null);
	}
	
	/**
	 * Draw the credential selection composite allowing selected domains, 
	 * or, if null, all domains. 
	 * 
	 * @param parent	The parent composite
	 * @param domains	The selected domains, or null to mean all domains
	 */
	public ChooseCredentialComponent(String[] domains) {
		this(domains, null);
	}
	
	/**
	 * 
	 * @param parent
	 * @param domains
	 * @param selectedUsername
	 */
	public ChooseCredentialComponent(String[] domains, String selectedUsername) {
		initialUsername = selectedUsername;
		domainList = findDomains(domains);
		CredentialService.getCredentialModel().addCredentialListener(this);
	}
	
	public void create(Composite parent) {
		parent.addDisposeListener(new DisposeListener() {
			@Override
			public void widgetDisposed(DisposeEvent e) {
				CredentialService.getCredentialModel().removeCredentialListener(ChooseCredentialComponent.this);
			}
		});
		createWidgets(parent);
		addWidgetListeners();
		refreshUserCombo(true);
	}
	
	protected void createWidgets(Composite parent) {
		domainLabel = new Label(parent, SWT.NONE);
		domainLabel.setText("Domain: ");
		
		domainCombo = new Combo(parent, SWT.READ_ONLY);
		domainCombo.setItems(findDomainNames(domainList));
		domainCombo.select(0);

		
		usernameLabel = new Label(parent, SWT.NONE);
		usernameLabel.setText("Username: ");
		
		userCombo = new Combo(parent, SWT.READ_ONLY);
		
		
		Composite editAddParent = parent;
		if( showEditButton()) {
			editAddParent = new Composite(parent, SWT.NONE);
			GridLayout gl = new GridLayout(2, true);
			editAddParent.setLayout(gl);
			editUser = new Button(editAddParent, SWT.PUSH);
			editUser.setText("Edit...");
			GridData editAddParentData = new GridData();
			editAddParentData.grabExcessHorizontalSpace = true;
			editAddParentData.horizontalAlignment = SWT.FILL;
			editAddParent.setLayoutData(editAddParentData);
			editUser.setLayoutData(editAddParentData);
		}
		
		addUser = new Button(editAddParent, SWT.PUSH);
		addUser.setText("Add...");
		
		if( domainList.size() == 1 ) {
			domainCombo.setEnabled(false);
		}
	}

	protected void addWidgetListeners() {

		// Adding listeners
		domainComboListener = new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				refreshUserCombo();
			}
		};
		domainCombo.addSelectionListener(domainComboListener);
		
		userComboListener = new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				fireChanged();
			}
		};
		userCombo.addSelectionListener(userComboListener);

		addUser.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				addUserPressed();
			}
		});
		if( editUser != null ) {
			editUser.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					editUserPressed();
				}
			});
		}
	}
	
	private void editUserPressed() {
		NewCredentialUserDialog dialog = new NewCredentialUserDialog(
				domainCombo.getShell(), CredentialService.getCredentialModel(), getDomain(), getUser());
		if( dialog.open() == Window.OK) {
			CredentialService.getCredentialModel().removeCredentials(getDomain(), getUser());
			credentialDialogOkPressed(dialog);
		}
	}
	
	private void addUserPressed() {
		NewCredentialUserDialog dialog = new NewCredentialUserDialog(
				domainCombo.getShell(),
				CredentialService.getCredentialModel(), getDomain());
		if( dialog.open() == Window.OK) {
			credentialDialogOkPressed(dialog);
		}
	}
	
	private void credentialDialogOkPressed(NewCredentialUserDialog dialog) {
		ICredentialDomain cd = dialog.getDomain();
		String name = dialog.getUser();
		String pass = dialog.getPass();
		if( dialog.isAlwaysPrompt()) {
			CredentialService.getCredentialModel().addPromptedCredentials(cd, name);
		} else {
			CredentialService.getCredentialModel().addCredentials(cd, name, pass);
		}
		CredentialService.getCredentialModel().save();
		refreshUserCombo(name, true);
	}
	
	private void refreshUserCombo() {
		refreshUserCombo(false);
	}
	
	private void refreshUserCombo(boolean initial) {
		if( initial ) {
			refreshUserCombo(initialUsername, false);
		} else {
			refreshUserCombo(null, true);
		}
	}
	private void refreshUserCombo(String user, boolean fire) {
		if(userCombo == null || userCombo.isDisposed())
			return;

		
		
		ICredentialDomain d = getDomain();
		if( d != null ) {
			String[] userNames = d.getUsernames();
			userCombo.setItems(userNames);
			if( userNames.length > 0 ) {
				if( user == null ) {
					userCombo.select(0);
				} else {
					int ind = Arrays.asList(userNames).indexOf(user);
					if( ind == -1 ) {
						userCombo.select(0);
					} else {
						userCombo.select(ind);
					}
				}
			}
		} 
		if( editUser != null ) 
			editUser.setEnabled(userCombo.getSelectionIndex() != -1);
		
		if( fire )
			fireChanged();
		userCombo.getParent().layout(false, false);
		userCombo.getParent().update();
	}
	
	protected void fireChanged() {
		Iterator<ICredentialCompositeListener> it = listeners.iterator();
		while(it.hasNext()) {
			it.next().credentialsChanged();
		}
	}
	
	
	protected boolean showEditButton() {
		return true;
	}

	
	private String[] findDomainNames(List<ICredentialDomain> domains) {
		ArrayList<String> ret = new ArrayList<String>();
		Iterator<ICredentialDomain> it = domains.iterator();
		while(it.hasNext()) {
			String name = it.next().getName();
			ret.add(name);
		}
		return (String[]) ret.toArray(new String[ret.size()]);
	}
	
	private ArrayList<ICredentialDomain> findDomains(String[] domains) {
		ArrayList<ICredentialDomain> list = new ArrayList<ICredentialDomain>();
		if( domains == null ) {
			ICredentialDomain[] domainArr = CredentialService.getCredentialModel().getDomains();
			list.addAll(Arrays.asList(domainArr));
		} else {
			for( int i = 0; i < domains.length; i++ ) {
				ICredentialDomain domain = CredentialService.getCredentialModel().getDomain(domains[i]);
				if( domain != null ) {
					list.add(domain);
				}
			}
		}
		return list;
	}
	
	public void addCredentialListener(ICredentialCompositeListener listener) {
		listeners.add(listener);
	}
	public void removeCredentialListener(ICredentialCompositeListener listener) {
		listeners.remove(listener);
	}
	
	public ICredentialDomain getDomain() {
		int ind = domainCombo.getSelectionIndex();
		if( ind != -1 ) {
			ICredentialDomain d = domainList.get(ind);
			return d;
		}
		return null;
	}
	
	public String getUser() {
		int userIndex = userCombo.getSelectionIndex();
		return userIndex == -1 ? null : userCombo.getItem(userIndex);
	}
	
	public String getPassword() {
		ICredentialDomain cd = getDomain();
		if( cd != null ) {
			try {
				return cd.getPassword(getUser());
			} catch(StorageException se) {
				FoundationUIPlugin.pluginLog().logError(se);
			}
		}
		return null;
	}
	public void setEnabled (boolean enabled) {
		if( !enabled ) {
			domainCombo.setEnabled(false);
			userCombo.setEnabled(false);
		} else {
			domainCombo.setEnabled(domainList.size() != 1);
			userCombo.setEnabled(true);
		}
	}
	
	
	/**
	 * To be called only after create
	 * Lays out the widgets into a pre-existing grid layout with n columns
	 * @param columns
	 */
	public void gridLayout(int n) {
		if( n == 2 ) {
			// We're in a 2 column grid
			GridData gd2 = new GridData();
			gd2.widthHint = 200;
			domainCombo.setLayoutData(gd2);
			userCombo.setLayoutData(gd2);
			GridData gd3 = new GridData();
			gd3.horizontalSpan = 2;
			addUser.setLayoutData(gd3);
		} else if( n >= 3 ) {
			// We're in a three column grid
			GridData domainData = new GridData();
			domainData.widthHint = 200;
			domainData.horizontalSpan = n-1;
			domainData.horizontalAlignment = SWT.FILL;
			domainCombo.setLayoutData(domainData);

			GridData userData = new GridData();
			userData.widthHint = 200;
			userData.horizontalSpan = n - 2;
			userData.grabExcessHorizontalSpace = true;
			userData.horizontalAlignment = SWT.FILL;
			userCombo.setLayoutData(userData);
			
			GridData gd3 = new GridData();
			gd3.horizontalAlignment = SWT.FILL;
			addUser.setLayoutData(gd3);
		}
	}
	

	public Combo getUserCombo() {
		return userCombo;
	}
	public Combo getDomainCombo() {
		return domainCombo;
	}
	
	public SelectionListener getUserListener() {
		return userComboListener;
	}
	public SelectionListener getDomainListener() {
		return domainComboListener;
	}

	@Override
	public void domainAdded(ICredentialDomain domain) {
		refreshUserCombo();
	}

	@Override
	public void domainRemoved(ICredentialDomain domain) {
		refreshUserCombo();
	}

	@Override
	public void defaultUsernameChanged(ICredentialDomain domain, String user) {
		refreshUserCombo();
	}

	@Override
	public void credentialAdded(ICredentialDomain domain, String user) {
		refreshUserCombo();
	}

	@Override
	public void credentialRemoved(ICredentialDomain domain, String user) {
		refreshUserCombo();
	}

	@Override
	public void credentialChanged(ICredentialDomain domain, String user) {
		refreshUserCombo();
	}
	
}
