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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.jboss.tools.foundation.core.credentials.UsernameChangedException;
import org.jboss.tools.foundation.core.internal.FoundationCorePlugin;
import org.jboss.tools.foundation.core.credentials.CredentialService;
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
	static final String PROPERTY_PROMPTED_USER_LIST = "user.list.prompted";
	
	static final String NOT_LOADED_PASSWORD = "********";
	
	
	private String userVisibleName;
	private String id, defaultUsername;
	private boolean removable;
	private HashMap<String, String> credentials;
	private ArrayList<String> promptedCredentials;
	public CredentialDomain(String id, String name, boolean removable) {
		if(id == null) {
			throw new IllegalArgumentException("Id cannot be null.");
		}
		this.id = id;
		this.userVisibleName = name;
		this.removable = removable;
		this.defaultUsername = null;
		this.credentials = new HashMap<String, String>();
		this.promptedCredentials = new ArrayList<String>();
	}
	
	public CredentialDomain(Preferences pref) throws BackingStoreException {
		this.id = pref.get(PROPERTY_ID, ""); //Id cannot be null.
		this.userVisibleName = pref.get(PROPERTY_NAME, (String)null);;
		this.removable = pref.getBoolean(PROPERTY_REMOVABLE, true);
		this.defaultUsername = pref.get(PROPERTY_DEFAULT_USER, (String)null);

		credentials = new HashMap<String, String>();
		String usersList = pref.get(PROPERTY_USER_LIST, (String)null);
		if( usersList != null && !usersList.isEmpty()) {
			String[] users = (usersList == null ? new String[0] : usersList.split("\n"));
			for( int i = 0; i < users.length; i++ ) {
				credentials.put(users[i], NOT_LOADED_PASSWORD);
			}
		}
		
		String promptedUserList = pref.get(PROPERTY_PROMPTED_USER_LIST, (String)null);
		promptedCredentials = new ArrayList<String>();
		if(promptedUserList != null && !promptedUserList.isEmpty()) {
			String[] users = (usersList == null ? new String[0] : promptedUserList.split("\n"));
			promptedCredentials.addAll(Arrays.asList(users));
		}
		
		if( defaultUsername == null || !userExists(defaultUsername)) {
			// The default name doesn't exist, so we need another. 
			String[] users = getUsernames();
			if( users.length > 0) {
				defaultUsername = users[0];
			}
		}
	}
	public String getId() {
		return id;
	}
	public boolean getRemovable() {
		return removable;
	}

	/**
	 * Since returned value is used by UI, returning null may cause NPE, 
	 * and therefore should be checked for null at each call.
	 * It is better not to return null.  
	 */
	@Override
	public String getName() {
		return emptyOrNull(userVisibleName) ? (emptyOrNull(id) ? "" : id) : userVisibleName;
	}
	
	private boolean emptyOrNull(String s) {
		return s == null ? true : s.isEmpty();
	}
	
	public boolean userExists(String user) {
		return credentials.containsKey(user) || promptedCredentials.contains(user);
	}
	
	public boolean userRequiresPrompt(String user) {
		return promptedCredentials.contains(user);
	}
	
	public String[] getUsernames() {
		SortedSet<String> ret = new TreeSet<String>();
		ret.addAll(credentials.keySet());
		ret.addAll(promptedCredentials);
		return (String[]) ret.toArray(new String[ret.size()]);
	}
	
	protected void addCredentials(String user, String pass) {
		if( defaultUsername == null ) 
			defaultUsername = user;
		promptedCredentials.remove(user);
		credentials.put(user, pass);
	}

	protected void addPromptedCredentials(String user) {
		if( defaultUsername == null ) 
			defaultUsername = user;
		credentials.remove(user);
		promptedCredentials.add(user);
	}

	protected void removeCredential(String user) {
		credentials.remove(user);
		promptedCredentials.remove(user);
		if( user.equals(defaultUsername)) {
			String[] usernames = getUsernames();
			if( usernames.length == 0 ) {
				defaultUsername = null;
			} else {
				defaultUsername = usernames[0];
			}
		}
	}
	
	public String getCredentials(String user) throws StorageException, UsernameChangedException {
		return getCredentials(user, true);
	}
	
	public String getPassword(String user) throws StorageException {
		try {
			return getCredentials(user, false);
		} catch(UsernameChangedException uce) {
			// Should never happen
			FoundationCorePlugin.pluginLog().logError("User attempted to change username when not allowed", uce);
		}
		return null;
	}
	

	public String getCredentials(String user, boolean canChangeUser) throws StorageException, UsernameChangedException {
		if( userExists(user)) {
			if( !userRequiresPrompt(user)) {
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
		} 
		
		if( canChangeUser ) {
			return CredentialsModel.getDefault().promptForCredentials(this, user);
		} else if( user != null){
			return CredentialsModel.getDefault().promptForPassword(this, user);
		} else {
			return null;
		}
	}
	
	private String getCredentialsForSave(String user) {
		if( !userRequiresPrompt(user)) {
			String ret = credentials.get(user);
			if( NOT_LOADED_PASSWORD.equals(ret)) {
				return null;
			}
			return ret;
		} else {
			return null;
		}
	}
	
	
	void saveToPreferences(Preferences prefs, ISecurePreferences securePrefs) throws StorageException {
		prefs.put(PROPERTY_ID, id);
		prefs.put(PROPERTY_NAME, getName());
		prefs.putBoolean(PROPERTY_REMOVABLE, removable);
		if( defaultUsername != null ) 
			prefs.put(PROPERTY_DEFAULT_USER, defaultUsername);
		
		Set<String> users = credentials.keySet();
		String[] userList = (String[]) users.toArray(new String[users.size()]);
		prefs.put(PROPERTY_USER_LIST, String.join("\n", userList));
		
		String[] childNodes = securePrefs.childrenNames();
		for( int i = 0; i < childNodes.length; i++ ) {
			// Delete old nodes that are no longer in the model
			ISecurePreferences userNode = securePrefs.node(childNodes[i]);
			if( !users.contains(childNodes[i])) {
				userNode.removeNode();
			} else {
				// Goal here is to force a secure-storage event
				userNode.get(PROPERTY_PASS, (String)null);
			}
		}
		
		// Save the password securely
		for( int i = 0; i < userList.length; i++ ) {
			String user = userList[i];
			ISecurePreferences userNode = securePrefs.node(user);
			String forSave = getCredentialsForSave(user);
			if( forSave != null ) {
				userNode.put(PROPERTY_PASS, forSave, true);
			}
		}
		
		String[] promptedUsers = (String[]) promptedCredentials.toArray(new String[promptedCredentials.size()]);
		prefs.put(PROPERTY_PROMPTED_USER_LIST, String.join("\n", promptedUsers));
	}

	public String getDefaultUsername() {
		return defaultUsername;
	}
	
	public void setDefaultUsername(String user) throws IllegalArgumentException {
		if( !userExists(user)) {
			throw new IllegalArgumentException("User " + user + " does not exist for this domain.");
		}
		defaultUsername = user;
	}
}