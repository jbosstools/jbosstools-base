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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.jboss.tools.foundation.core.credentials.CredentialService;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.ui.credentials.internal.NewCredentialUserDialog;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;

public class ChooseCredentialComposite extends Composite {
	protected ArrayList<ICredentialDomain> domainList;
	protected Combo domainCombo, userCombo;
	protected ArrayList<ICredentialCompositeListener> listeners = new ArrayList<ICredentialCompositeListener>();
	protected String initialUsername;
	protected SelectionListener userComboListener, domainComboListener;
	
	/**
	 * Draw the credential selection composite allowing all domains
	 */
	public ChooseCredentialComposite(Composite parent) {
		this(parent, null);
	}
	
	/**
	 * Draw the credential selection composite allowing selected domains, 
	 * or, if null, all domains. 
	 * 
	 * @param parent	The parent composite
	 * @param domains	The selected domains, or null to mean all domains
	 */
	public ChooseCredentialComposite(Composite parent, String[] domains) {
		this(parent, domains, null);
	}
	
	/**
	 * 
	 * @param parent
	 * @param domains
	 * @param selectedUsername
	 */
	public ChooseCredentialComposite(Composite parent, String[] domains, String selectedUsername) {
		super(parent, SWT.NONE);
		initialUsername = selectedUsername;
		setLayout(new GridLayout(2, false));
		
		domainList = findDomains(domains);

		Label domainLabel = new Label(this, SWT.NONE);
		domainLabel.setText("Domain: ");
		
		domainCombo = new Combo(this, SWT.READ_ONLY);
		domainCombo.setItems(findDomainNames(domainList));
		domainCombo.select(0);

		
		Label usernameLabel = new Label(this, SWT.NONE);
		usernameLabel.setText("Username: ");
		
		userCombo = new Combo(this, SWT.READ_ONLY);
		
		Link addUser = new Link(this, SWT.None);
		addUser.setText("<a>Add Credential...</a>");
		
		GridData gd2 = new GridData();
		gd2.widthHint = 200;
		domainCombo.setLayoutData(gd2);
		userCombo.setLayoutData(gd2);
		
		if( domainList.size() == 1 ) {
			domainCombo.setEnabled(false);
		}
		
		
		
		
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

		
		refreshUserCombo(true);
		
	}

	private void addUserPressed() {
		NewCredentialUserDialog dialog = new NewCredentialUserDialog(
				domainCombo.getShell(),
				CredentialService.getCredentialModel(), getDomain());
		if( dialog.open() == Window.OK) {
			ICredentialDomain cd = dialog.getDomain();
			String name = dialog.getUser();
			String pass = dialog.getPass();
			CredentialService.getCredentialModel().addCredentials(cd, name, pass);
			CredentialService.getCredentialModel().saveModel();
			refreshUserCombo(name, true);
		}
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
		if( fire )
			fireChanged();
	}
	
	private void fireChanged() {
		Iterator<ICredentialCompositeListener> it = listeners.iterator();
		while(it.hasNext()) {
			it.next().credentialsChanged();
		}
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
				return cd.getCredentials(getUser());
			} catch(StorageException se) {
				FoundationUIPlugin.pluginLog().logError(se);
			}
		}
		return null;
	}
	public void setEnabled (boolean enabled) {
		super.setEnabled(enabled);
		if( !enabled ) {
			domainCombo.setEnabled(false);
			userCombo.setEnabled(false);
		} else {
			domainCombo.setEnabled(domainList.size() != 1);
			userCombo.setEnabled(true);
		}
	}
}
