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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.jboss.tools.common.oauth.core.CommonOAuthCoreConstants;
import org.jboss.tools.common.oauth.core.model.IAccount;
import org.jboss.tools.common.oauth.core.model.IAuthorizationServer;
import org.jboss.tools.common.oauth.internal.core.CommonOAuthCoreActivator;
import org.jboss.tools.common.oauth.internal.core.model.AccountModel.Event;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class AuthorizationServer implements IAuthorizationServer {

  private static final String REALM_KEY = "realm";
  private static final String URL_KEY = "url";
  private static final String CLIENT_ID_KEY = "clientId";

	private String id;

	private String realm;

	private String url;
	
	private String clientId;
	
	private String displayName;

	private List<IAccount> identities = new ArrayList<>();

	private List<String> removed = new ArrayList<>();

	private AccountModel model;

	public AuthorizationServer(AccountModel model, String id) {
		this.model = model;
		this.id = id;
	}

	@Override
	public String getId() {
		return id;
	}

	/**
   * @return the realm
   */
  public String getRealm() {
    return realm;
  }

  /**
   * @param realm the realm to set
   */
  public void setRealm(String realm) {
    this.realm = realm;
  }

  @Override
	public String getURL() {
		return url;
	}

	@Override
	public void setURL(String url) {
		this.url = url;
	}

	/**
   * @return the clientId
   */
  public String getClientId() {
    return clientId;
  }

  /**
   * @param clientId the clientId to set
   */
  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  /**
   * @return the displayName
   */
  public String getDisplayName() {
    if (displayName == null) {
      return getId();
    }
    return displayName;
  }

  /**
   * @param displayName the displayName to set
   */
  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  @Override
	public void addAccount(IAccount account) {
		identities.add(account);
		model.fireEvent(Event.ACCOUNT_ADDED, account);
	}

	@Override
	public List<IAccount> getAccounts() {
		return identities;
	}

	@Override
	public void removeAccount(IAccount account) {
		identities.remove(account);
		removed.add(account.getId());
		model.fireEvent(Event.ACCOUNT_REMOVED, account);
	}

	@Override
	public IAccount createAccount(String id) {
		return new Account(id, this);
	}

	@Override
	public void save() {
		try {
			Preferences accountsNode = AccountModel.getAccountsPreferences();
			Preferences serverNode = accountsNode.node(getId());
			ISecurePreferences serverSecureNode = AccountModel.getSecureAccountsPreferences().node(getId());
			serverNode.put(REALM_KEY, getRealm());
			serverNode.put(URL_KEY, getURL());
			serverNode.put(CLIENT_ID_KEY, getClientId());
			removed.forEach(id -> {
				try {
					serverNode.node(id).removeNode();
					serverSecureNode.node(id).removeNode();
				} catch (BackingStoreException e) {
					CommonOAuthCoreActivator.logError(e.getLocalizedMessage(), e);
				}
			});
			removed.clear();

			serverNode.flush();
			serverSecureNode.flush();
		} catch (BackingStoreException | IOException e) {
			CommonOAuthCoreActivator.logError("Error saving authorization server in storage", e);
		}
	}

	public void load(IConfigurationElement element, Preferences authorizationServerNode, ISecurePreferences secureAuthorizationServerNode) throws StorageException {
    this.setRealm(element.getAttribute(CommonOAuthCoreConstants.REALM_ATTRIBUTE_NAME));
    this.setURL(element.getAttribute(CommonOAuthCoreConstants.URL_ATTRIBUTE_NAME));
    this.setClientId(element.getAttribute(CommonOAuthCoreConstants.CLIENT_ID_ATTRUBUTE_NAME));
    this.setDisplayName(element.getAttribute(CommonOAuthCoreConstants.DISPLAY_NAME_ATTRUBUTE_NAME));

    try {
      String[] ids = authorizationServerNode.childrenNames();
      for (String id : ids) {
        Account account = new Account(id, this);
        Preferences accountNode = authorizationServerNode.node(id);
        ISecurePreferences secureAccountNode = secureAuthorizationServerNode.node(id);
        try {
          account.load(accountNode, secureAccountNode);
          identities.add(account);
        } catch (StorageException e) {
          CommonOAuthCoreActivator.logError(e.getLocalizedMessage(), e);
        }
      }
    } catch (BackingStoreException e) {
      CommonOAuthCoreActivator.logError(e.getLocalizedMessage(), e);
    }
	}

  public static IConfigurationElement[] getAuthorizationServers() {
  	return Platform.getExtensionRegistry()
  			.getConfigurationElementsFor(CommonOAuthCoreConstants.AUTHORIZATION_SERVER_EXTENSION_POINT);
  }
}
