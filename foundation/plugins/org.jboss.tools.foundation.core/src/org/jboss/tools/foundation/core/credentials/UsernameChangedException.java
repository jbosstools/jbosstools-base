/******************************************************************************* 
 * Copyright (c) 2016 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.core.credentials;

/**
 * This exception indicates that while prompting the user for the 
 * credentials of a specific user, the user chose to change the username.
 * 
 */
public class UsernameChangedException extends Exception {
	private static final long serialVersionUID = 1L;
	private ICredentialDomain initialDomain;
	private String initialUser;
	private String user;
	private String password;
	private boolean saveCredentials;

	public UsernameChangedException(ICredentialDomain initialDomain, String initialUser, 
			String user, String password, boolean saveCredentials) {
		super();
		this.initialDomain = initialDomain;
		this.initialUser = initialUser;
		this.password = password;
		this.user = user;
		this.saveCredentials = saveCredentials;
	}

	public ICredentialDomain getInitialDomain() {
		return initialDomain;
	}

	public String getInitialUser() {
		return initialUser;
	}

	public String getUser() {
		return user;
	}
	
	public String getPassword() {
		return password;
	}

	public boolean getSaveCredentials() {
		return saveCredentials;
	}

	
}
