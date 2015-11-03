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
package org.jboss.tools.foundation.core.credentials.internal;

import java.util.HashMap;
import java.util.Set;

import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class CredentialDomain implements ICredentialDomain {

	// Properties used in the secure storage model
	static final String PROPERTY_ID = "id";
	static final String PROPERTY_NAME = "name";
	static final String PROPERTY_REMOVABLE = "removable";
	static final String PROPERTY_PASS = "pass";
	static final String PROPERTY_DEFAULT_USER = "default.user";
	static final String PROPERTY_USER_LIST = "user.list";
	
	static final String NOT_LOADED_PASSWORD = "********";
	
	
	private String userVisibleName;
	private String id, defaultUsername;
	private boolean removable;
	private HashMap<String, String> credentials;
	public CredentialDomain(String id, String name, boolean removable) {
		this.id = id;
		this.userVisibleName = name;
		this.removable = removable;
		this.defaultUsername = null;
		credentials = new HashMap<String, String>();
	}
	
	public CredentialDomain(Preferences pref) throws BackingStoreException {
		this.id = pref.get(PROPERTY_ID, (String)null);
		this.userVisibleName = pref.get(PROPERTY_NAME, (String)null);;
		this.removable = pref.getBoolean(PROPERTY_REMOVABLE, true);
		this.defaultUsername = pref.get(PROPERTY_DEFAULT_USER, (String)null);
		
		credentials = new HashMap<String, String>();
		String usersList = pref.get(PROPERTY_USER_LIST, (String)null);
		if( !usersList.isEmpty()) {
			String[] users = (usersList == null ? new String[0] : usersList.split("\n"));
			for( int i = 0; i < users.length; i++ ) {
				credentials.put(users[i], NOT_LOADED_PASSWORD);
			}
		}
		if( defaultUsername == null && credentials.size() > 0 ) {
			defaultUsername = credentials.keySet().iterator().next();
		}
	}
	public String getId() {
		return id;
	}
	public boolean getRemovable() {
		return removable;
	}
	public String getName() {
		return userVisibleName;
	}
	
	public boolean userExists(String user) {
		return credentials.containsKey(user);
	}
	
	public String[] getUsernames() {
		Set<String> keys = credentials.keySet();
		return (String[]) keys.toArray(new String[keys.size()]);
	}
	
	protected void addCredentials(String user, String pass) {
		if( defaultUsername == null ) 
			defaultUsername = user;
		credentials.put(user, pass);
	}
	
	protected void removeCredential(String user) {
		credentials.remove(user);
		if( user.equals(defaultUsername)) {
			// We've removed our default, find a new one
			if( credentials.size() == 0 ) {
				defaultUsername = null;
			} else {
				// Take the first from the list
				defaultUsername = credentials.keySet().iterator().next();
			}
		}
	}
	
	public String getCredentials(String user) throws StorageException {
		String ret = credentials.get(user);
		if( NOT_LOADED_PASSWORD.equals(ret)) {
			ISecurePreferences secureRoot = SecurePreferencesFactory.getDefault();
			ISecurePreferences secureCredentialRoot = secureRoot.node(CredentialsModel.CREDENTIAL_BASE_KEY);
			ISecurePreferences secureDomain = secureCredentialRoot.node(getId());
			ISecurePreferences secureUser = secureDomain.node(user);
			ret = secureUser.get(PROPERTY_PASS, (String)null);
			credentials.put(user,  ret);
		}
		return ret;
	}

	void saveToPreferences(Preferences prefs, ISecurePreferences securePrefs) throws StorageException {
		prefs.put(PROPERTY_ID, id);
		prefs.put(PROPERTY_NAME, userVisibleName);
		prefs.putBoolean(PROPERTY_REMOVABLE, removable);
		if( defaultUsername != null ) 
			prefs.put(PROPERTY_DEFAULT_USER, defaultUsername);
		
		Set<String> users = credentials.keySet();
		String[] userList = (String[]) users.toArray(new String[users.size()]);
		prefs.put(PROPERTY_USER_LIST, String.join("\n", userList));
		
		// Save the password securely
		for( int i = 0; i < userList.length; i++ ) {
			String user = userList[i];
			ISecurePreferences userNode = securePrefs.node(user);
			userNode.put(PROPERTY_PASS, getCredentials(user), true);
		}
	}
	@Override
	public String getDefaultUsername() {
		return defaultUsername;
	}
	
	public void setDefaultUsername(String user) throws IllegalArgumentException {
		if( !credentials.containsKey(user)) {
			throw new IllegalArgumentException("User " + user + " does not exist for this domain.");
		}
		defaultUsername = user;
	}
}