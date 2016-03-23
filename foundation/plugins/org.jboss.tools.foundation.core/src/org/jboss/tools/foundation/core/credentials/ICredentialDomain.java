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

import org.eclipse.equinox.security.storage.StorageException;

/**
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface ICredentialDomain {

	/**
	 * Get the internal id for this credential domain
	 * @return
	 */
	public String getId();
	
	/**
	 * Get whether this credential domain can be removed by the user or is permanent.
	 * @return
	 */
	public boolean getRemovable();
	
	/**
	 * Get the user-visible name of this credential domain
	 * Does not return null, to prevent necessity to check for null at each call,
	 * since most UI clients calling this methods do not allow null. 
	 *   
	 * @return 
	 */
	public String getName();
	
	/**
	 * Does the given username exist in the model
	 * @param user
	 * @return
	 */
	public boolean userExists(String user);
	
	/**
	 * Get a list of usernames persisted for this domain
	 * @return
	 */
	public String[] getUsernames();
	
	/**
	 * Get the password for the given username
	 * @param user
	 * @return
	 * @throws StorageException
	 * @throws UsernameChangedException if the user has changed the username when prompted 
	 */
	public String getCredentials(String user) throws StorageException, UsernameChangedException;
	
	/**
	 * Get the password for the given username. 
	 * The user has no opportunity to change the username.
	 * 
	 * @param user
	 * @return
	 * @throws StorageException
	 */
	public String getPassword(String user) throws StorageException;
	
	/**
	 * Get the default username for this domain, if one exists, or null
	 * @return
	 */
	public String getDefaultUsername();
}
