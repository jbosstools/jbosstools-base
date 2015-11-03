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
 * A listener to register with the credential framework to stay 
 * aware of changes to the credential model. 
 * 
 * Since this API is new, it is likely to change. We strongly suggest 
 * extending the CredentialAdapter instead of implementing this interface directly.
 */
public interface ICredentialListener {
	
	/**
	 * A domain has been added to the model
	 * @param domain
	 */
	public void domainAdded(ICredentialDomain domain);
	
	/**
	 * A domain has been removed from the model
	 * @param domain
	 */
	public void domainRemoved(ICredentialDomain domain);
	
	/**
	 * The default username for the given domain has been changed
	 * @param domain
	 * @param user
	 */
	public void defaultUsernameChanged(ICredentialDomain domain, String user);
	
	/**
	 * A credential has been added to the model
	 * @param domain
	 * @param user
	 */
	public void credentialAdded(ICredentialDomain domain, String user);
	
	/**
	 * A user/pass combination has been removed from the model
	 * @param domain
	 * @param user
	 */
	public void credentialRemoved(ICredentialDomain domain, String user);
	
	/**
	 * Something about this credential has changed, most likely the password.
	 * This event may be fired even if the password has stayed the same. 
	 * No verification is done to make sure the new password differs from the old. 
	 * @param domain
	 * @param user
	 */
	public void credentialChanged(ICredentialDomain domain, String user);
	
}
