/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.internal.core.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.jboss.tools.common.oauth.core.CommonOAuthCoreConstants;
import org.jboss.tools.common.oauth.core.model.IAccount;
import org.jboss.tools.common.oauth.core.model.IAccountModel;
import org.jboss.tools.common.oauth.core.model.IAccountModelListener;
import org.jboss.tools.common.oauth.core.model.IAuthorizationServer;
import org.jboss.tools.common.oauth.internal.core.CommonOAuthCoreActivator;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class AccountModel implements IAccountModel {

	private List<IAuthorizationServer> authorizationServers = new ArrayList<>();

	private List<String> removed = new ArrayList<>();

	private List<IAccountModelListener> listeners = new ArrayList<>();

	enum Event {
		ACCOUNT_ADDED, ACCOUNT_REMOVED, SERVER_ADDED, SERVER_REMOVED
	}

	public AccountModel() {
		loadModel();
	}

	private void loadModel() {
		ISecurePreferences secureAccountRoot = getSecureAccountsPreferences();
		Preferences accountsRoot = getAccountsPreferences();
			IConfigurationElement[] servers = AuthorizationServer.getAuthorizationServers();
			for (IConfigurationElement server : servers) {
			  String id = server.getAttribute(CommonOAuthCoreConstants.ID_ATTRIBUTE_NAME);
				AuthorizationServer authorizationServer = new AuthorizationServer(this, id);
				Preferences authorizationServerNode = accountsRoot.node(id);
				ISecurePreferences secureAuthorizationServerNode = secureAccountRoot.node(id);
				addAuthorizationServer(authorizationServer, server, authorizationServerNode, secureAuthorizationServerNode);
			}
	}

	void addAuthorizationServer(AuthorizationServer server, IConfigurationElement element, Preferences authorizationServerNode, ISecurePreferences secureAuthorizationServerNode) {
		try {
			server.load(element, authorizationServerNode, secureAuthorizationServerNode);
			authorizationServers.add(server);
		} catch (StorageException e) {
			CommonOAuthCoreActivator.logError(e.getLocalizedMessage(), e);
		}
	}

	void fireEvent(Event event, IAccount account) {
		listeners.forEach(listener -> {
			switch (event) {
			case ACCOUNT_ADDED:
				listener.accountAdded(this, account);
				break;
			case ACCOUNT_REMOVED:
				listener.accountRemoved(this, account);
				break;
			}
		});
	}

	void fireEvent(Event event, IAuthorizationServer server) {
		listeners.forEach(listener -> {
			switch (event) {
			case SERVER_ADDED:
				listener.authorizationServerAdded(this, server);
				break;
			case SERVER_REMOVED:
				listener.authorizationRemoved(this, server);
				break;
			}
		});
	}

	@Override
	public IAuthorizationServer createAuthorizationServer(String id) {
		return new AuthorizationServer(this, id);
	}

	@Override
	public void addAuthorizationServer(IAuthorizationServer server) {
		authorizationServers.add(server);
		fireEvent(Event.SERVER_ADDED, server);
	}

	@Override
	public List<IAuthorizationServer> getAuthorizationServers() {
		return authorizationServers;
	}

	@Override
	public void removeAuthorizationServer(IAuthorizationServer server) {
		authorizationServers.remove(server);
		fireEvent(Event.SERVER_REMOVED, server);
		removed.add(server.getId());
	}

	@Override
	public void save() {
		authorizationServers.forEach(IAuthorizationServer::save);
		Preferences accountRoot = getAccountsPreferences();
		ISecurePreferences accountSecureRoot = getSecureAccountsPreferences();
		removed.forEach(id -> {
			removeAccount(id, accountRoot, accountSecureRoot);
		});
		removed.clear();
	}

	void removeAccount(String id, Preferences accountRoot, ISecurePreferences accountSecureRoot) {
		try {
			accountRoot.node(id).removeNode();
			accountSecureRoot.node(id).removeNode();
		} catch (BackingStoreException e) {
			CommonOAuthCoreActivator.logError(e.getLocalizedMessage(), e);
		}
	}

	@Override
	public void addListener(IAccountModelListener listener) {
		listeners.add(listener);
	}

	@Override
	public void removeListener(IAccountModelListener listener) {
		listeners.remove(listener);
	}

	static ISecurePreferences getSecureAccountsPreferences() {
		ISecurePreferences secureRoot = SecurePreferencesFactory.getDefault();
		return secureRoot.node(CommonOAuthCoreActivator.PLUGIN_ID).node(CommonOAuthCoreConstants.ACCOUNT_BASE_KEY);
	}

	static Preferences getAccountsPreferences() {
		IEclipsePreferences root = InstanceScope.INSTANCE.getNode(CommonOAuthCoreActivator.PLUGIN_ID);
		return root.node(CommonOAuthCoreConstants.ACCOUNT_BASE_KEY);
	}
}
