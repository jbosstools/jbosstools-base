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
 * The credential model, used to save username/password combinations for 
 * given domains. 
 * 
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface ICredentialsModel {

	/**
	 * Add a domain with the given id and name, and mark whether it is removable by users or not
	 * 
	 * @param id
	 * @param name
	 * @param removable
	 * @return 
	 */
	public ICredentialDomain addDomain(String id, String name, boolean removable);	
	
	/**
	 * Get a list of the domains
	 * @return
	 */
	public ICredentialDomain[] getDomains();
	
	/**
	 * Find a given credential domain
	 * 
	 * @param id
	 * @return
	 */
	public ICredentialDomain getDomain(String id);
	
	/**
	 * Remove a given credential domain
	 * @param domain
	 */
	public void removeDomain(ICredentialDomain domain);

	
	/**
	 * Add a credential to the provided domain
	 * @param domain
	 * @param user
	 * @param pass
	 */
	public void addCredentials(ICredentialDomain domain, String user, String pass);
	
	/**
	 * Add credentials that require a prompt on every occasion
	 * @param domain
	 * @param user
	 */
	public void addPromptedCredentials(ICredentialDomain domain, String user);
	
	
	/**
	 * Return whether this credential requires a prompt on every occasion
	 * @param domain
	 * @param user
	 * @return
	 */
	public boolean credentialRequiresPrompt(ICredentialDomain domain, String user);
	
	/**
	 * Remove a credential from the given credential domain
	 * @param domain
	 * @param user
	 */
	public void removeCredentials(ICredentialDomain domain, String user);
	
	/**
	 * Change the default username for this domain
	 * @param domain
	 * @param user
	 */
	public void setDefaultCredential(ICredentialDomain domain, String user);
	
	/**
	 * Add a credential listener to this framework
	 * @param listener
	 */
	public void addCredentialListener(ICredentialListener listener);
	
	/**
	 * Remove a credential listener
	 * @param listener
	 */
	public void removeCredentialListener(ICredentialListener listener);

	
	/**
	 * Save the credential model in secure storage
	 */
	public void saveModel();
}
