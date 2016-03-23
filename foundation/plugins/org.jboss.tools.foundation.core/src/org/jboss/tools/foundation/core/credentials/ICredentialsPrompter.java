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
package org.jboss.tools.foundation.core.credentials;

/**
 * Provide a password for the given domain / user combination
 */
public interface ICredentialsPrompter {
	
	/**
	 * Initialize the prompter with information
	 * @param domain
	 * @param user
	 */
	public void init(ICredentialDomain domain, String user, boolean canChangeUser);
	
	/**
	 * Prompt the user to complete the prompt
	 */
	public void prompt();
	
	/**
	 * Get the username
	 * @return
	 */
	public String getUsername();
	
	/**
	 * Get the password
	 * @return
	 */
	public String getPassword();
	
	/**
	 * 
	 * @return
	 */
	public boolean saveChanges();
	
}
