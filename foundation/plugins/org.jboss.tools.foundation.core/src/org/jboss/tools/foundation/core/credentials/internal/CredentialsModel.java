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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.jboss.tools.foundation.core.credentials.CredentialService;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.core.credentials.ICredentialListener;
import org.jboss.tools.foundation.core.credentials.ICredentialsModel;
import org.jboss.tools.foundation.core.internal.FoundationCorePlugin;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class CredentialsModel implements ICredentialsModel {
	private static CredentialsModel instance = new CredentialsModel();
	
	// A preference key where we store the string
	static final String CREDENTIAL_BASE_KEY = "org.jboss.tools.foundation.core.credentials.CredentialsModel";
	
	/*
	 * Internal event types
	 */
	private static final int DOMAIN_ADDED = 1;
	private static final int DOMAIN_REMOVED = 2;
	private static final int CREDENTIAL_ADDED = 3;
	private static final int CREDENTIAL_REMOVED = 4;
	private static final int CREDENTIAL_CHANGED = 5;
	private static final int DEFAULT_CREDENTIAL_CHANGED = 6;
	
	
	public static CredentialsModel getDefault() {
		return instance;
	}
	
	private IEclipsePreferences prefs;
	private HashMap<String, ICredentialDomain> map;
	private ArrayList<ICredentialListener> listeners;
	public CredentialsModel() {
		loadModel();
	}
	
	private void loadModel() {
		map = new HashMap<String, ICredentialDomain>();
		listeners = new ArrayList<>();
		try {
			ICredentialDomain[] domains = loadDomainsFromPreferences();
			for( int i = 0; i < domains.length; i++ ) {
				map.put(domains[i].getId(), domains[i]);
			}
			
			
			// Static domains that must always be present.  For now, hard-coded, but maybe
			// Can be contributed via ext-pt later. 
			if( !map.containsKey(CredentialService.REDHAT_ACCESS)) {
				map.put(CredentialService.REDHAT_ACCESS, new CredentialDomain(CredentialService.REDHAT_ACCESS, CredentialService.REDHAT_ACCESS, false));
			}
			if( !map.containsKey(CredentialService.JBOSS_ORG)) {
				map.put(CredentialService.JBOSS_ORG, new CredentialDomain(CredentialService.JBOSS_ORG, CredentialService.JBOSS_ORG, false));
			}
		} catch(BackingStoreException bse) {
			// TODO log
			bse.printStackTrace();
		}
	}
	
	
	/**
	 * Fire the events to listeners
	 * @param type
	 * @param domain
	 * @param user
	 */
	private void fireEvent(int type, ICredentialDomain domain, String user) {
		Iterator<ICredentialListener> it = listeners.iterator();
		while(it.hasNext()) {
			switch(type) {
			case DOMAIN_ADDED:
				it.next().domainAdded(domain);
				break;
			case DOMAIN_REMOVED:
				it.next().domainRemoved(domain);
				break;
			case CREDENTIAL_ADDED:
				it.next().credentialAdded(domain, user);
				break;
			case CREDENTIAL_REMOVED:
				it.next().credentialRemoved(domain, user);
				break;
			case CREDENTIAL_CHANGED:
				it.next().credentialChanged(domain, user);
				break;
			case DEFAULT_CREDENTIAL_CHANGED:
				it.next().defaultUsernameChanged(domain, user);
				break;
			}
		}
	}
	
	public void addCredentials(ICredentialDomain domain, String user, String pass) {
		CredentialDomain cd = (CredentialDomain)domain;
		boolean existed = cd.userExists(user);
		String preDefault = cd.getDefaultUsername();
		((CredentialDomain)domain).addCredentials(user, pass);
		String postDefault = cd.getDefaultUsername();
		
		// fire credential added or changed
		if( !existed )
			fireEvent(CREDENTIAL_ADDED, domain, user);
		else 
			fireEvent(CREDENTIAL_CHANGED, domain, user);
		
		
		if( !isEqual(preDefault, postDefault)) {
			fireEvent(DEFAULT_CREDENTIAL_CHANGED, domain, user);
		}
	}
		
	
	public void removeCredentials(ICredentialDomain domain, String user) {
		CredentialDomain cd = (CredentialDomain)domain;
		String preDefault = cd.getDefaultUsername();
		((CredentialDomain)domain).removeCredential(user);
		String postDefault = cd.getDefaultUsername();
		fireEvent(CREDENTIAL_REMOVED, domain, user);
		if( !isEqual(preDefault, postDefault)) {
			fireEvent(DEFAULT_CREDENTIAL_CHANGED, domain, user);
		}
	}

	private boolean isEqual(String one, String two) {
		if( one == null ) {
			return two == null;
		} else {
			return one.equals(two);
		}
	}
	
	public ICredentialDomain addDomain(String id, String name, boolean removable) {
		if( !map.containsKey(id)) {
			ICredentialDomain d = new CredentialDomain(id, name, removable);
			map.put(d.getId(), d);
			fireEvent(DOMAIN_ADDED, d, null);
			return d;
		}
		return null;
	}
	
	public ICredentialDomain[] getDomains() {
		ArrayList<ICredentialDomain> result = new ArrayList<ICredentialDomain>(map.values());
		Collections.sort(result, new Comparator<ICredentialDomain>() {
			public int compare(ICredentialDomain o1, ICredentialDomain o2) {
				return o1.getName().compareTo(o2.getName());
			}
		});
		return (ICredentialDomain[]) result.toArray(new ICredentialDomain[result.size()]);
	}
	
	public ICredentialDomain getDomain(String id) {
		return map.get(id);
	}
	
	public void removeDomain(ICredentialDomain domain) {
		if( domain != null && map.containsKey(domain.getId())) {
			map.remove(domain.getId());
			fireEvent(DOMAIN_REMOVED, domain, null);
		}
	}
	

	@Override
	public void setDefaultCredential(ICredentialDomain domain, String user) throws IllegalArgumentException {
		String original = ((CredentialDomain)domain).getDefaultUsername();
		if( user != null && !user.equals(original)) {
			((CredentialDomain)domain).setDefaultUsername(user);
			fireEvent(DEFAULT_CREDENTIAL_CHANGED, domain, user);
		}
	}

	
	private ICredentialDomain[] loadDomainsFromPreferences() throws BackingStoreException {
		ArrayList<ICredentialDomain> domains = new ArrayList<ICredentialDomain>();
		IEclipsePreferences root = InstanceScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID);
		Preferences credentialRoot = root.node(CREDENTIAL_BASE_KEY);
		String[] childNodes = credentialRoot.childrenNames();
		for( int i = 0; i < childNodes.length; i++ ) {
			Preferences domain = credentialRoot.node(childNodes[i]);
			ICredentialDomain cd = new CredentialDomain(domain);
			domains.add(cd);
		}
		return (ICredentialDomain[]) domains.toArray(new ICredentialDomain[domains.size()]);
	}
	
	
	
	public void saveModel() {
		try {
			ISecurePreferences secureRoot = SecurePreferencesFactory.getDefault();
			ISecurePreferences secureCredentialRoot = secureRoot.node(CREDENTIAL_BASE_KEY);
			IEclipsePreferences root = InstanceScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID);
			Preferences credentialRoot = root.node(CREDENTIAL_BASE_KEY);
			
			ArrayList<ICredentialDomain> domains = new ArrayList<ICredentialDomain>(map.values());
			Iterator<ICredentialDomain> it = domains.iterator();
			while(it.hasNext()) {
				ICredentialDomain d = it.next();
				ISecurePreferences secureDomainNode = secureCredentialRoot.node(d.getId());
				Preferences domainNode = credentialRoot.node(d.getId());
				((CredentialDomain)d).saveToPreferences(domainNode, secureDomainNode);
			}
			credentialRoot.flush();
			secureCredentialRoot.flush();
		} catch(StorageException se) {
			// TODO logging
			se.printStackTrace();
		} catch(IOException ioe) {
			// TODO logging
			ioe.printStackTrace();
		} catch(BackingStoreException bse) {
			// TODO logging
			bse.printStackTrace();
		}
	}
	
	IEclipsePreferences getPreferences() {
		if (prefs == null) {
			prefs = ConfigurationScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID);
		}
		return prefs;
	}

	@Override
	public void addCredentialListener(ICredentialListener listener) {
		listeners.add(listener);
	}

	@Override
	public void removeCredentialListener(ICredentialListener listener) {
		listeners.remove(listener);
	}
	
}
