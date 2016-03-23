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

import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.core.credentials.ICredentialsPrompter;

public class CredentialsPrompter implements ICredentialsPrompter {

	private ICredentialDomain domain;
	private String initialUser;
	private String selectedUser, selectedPassword;
	private boolean canChangeUser, saveChanges;
	
	public CredentialsPrompter() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void init(ICredentialDomain domain, String user, boolean canChangeUser) {
		this.domain = domain;
		this.initialUser = user;
		this.canChangeUser = canChangeUser;
	}

	@Override
	public void prompt() {
		final CredentialPromptDialog loginDialog = new CredentialPromptDialog(domain, initialUser, canChangeUser);
		final String[] result = new String[2];
		final Boolean[] saveChanges = new Boolean[1];
		PlatformUI.getWorkbench().getDisplay().syncExec(new Runnable() {
			public void run() {
				if (loginDialog.open() == Window.OK) {
					result[0] = loginDialog.getUser();
					result[1] = loginDialog.getPassword();
					saveChanges[0] = loginDialog.getSaveChanges();
				} else {
					result[0] = null;
					result[1] = null;
					saveChanges[0] = false;
				}
			}
		});
		selectedUser = result[0];
		selectedPassword = result[1];
		this.saveChanges = saveChanges[0];
	}

	@Override
	public String getUsername() {
		return selectedUser;
	}

	@Override
	public String getPassword() {
		return selectedPassword;
	}

	@Override
	public boolean saveChanges() {
		return saveChanges;
	}

}
