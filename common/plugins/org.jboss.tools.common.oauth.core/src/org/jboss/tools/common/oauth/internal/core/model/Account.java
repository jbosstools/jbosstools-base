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

import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.jboss.tools.common.oauth.core.TokenProvider;
import org.jboss.tools.common.oauth.core.model.IAccount;
import org.jboss.tools.common.oauth.core.model.IAuthorizationServer;
import org.jboss.tools.common.oauth.internal.core.CommonOAuthCoreActivator;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class Account implements IAccount {

	private static final String ACCESS_TOKEN_EXPIRY_TIME_KEY = "accessTokenExpiryTime";

	private static final String REFRESH_TOKEN_EXPIRY_TIME_KEY = "refreshTokenExpiryTime";

	private static final String LAST_REFRESHED_TIME_KEY = "lastRefreshTime";

	private static final String ID_TOKEN_KEY = "IDToken";
	
	private static final String ACCESS_TOKEN_KEY = "accessToken";

	private static final String REFRESH_TOKEN_KEY = "refreshToken";

	private String id;
	
	private String idToken;

	private String accessToken;

	private String refreshToken;

	private long accessTokenExpiryTime = Long.MAX_VALUE;

	private long refreshTokenExpiryTime = Long.MAX_VALUE;

	private long lastRefreshedTime;

	private IAuthorizationServer server;

	public Account(String id, IAuthorizationServer server) {
		this.id = id;
		this.server = server;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public IAuthorizationServer getAuthorizationServer() {
		return server;
	}

	/**
   * @return the idToken
   */
  public String getIDToken() {
    return idToken;
  }

  /**
   * @param idToken the idToken to set
   */
  public void setIDToken(String idToken) {
    this.idToken = idToken;
  }

  @Override
	public String getAccessToken() {
		return accessToken;
	}

	@Override
	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}

	@Override
	public String getRefreshToken() {
		return refreshToken;
	}

	@Override
	public void setRefreshToken(String refreshToken) {
		this.refreshToken = refreshToken;
	}

	@Override
  public String getToken(int tokenType) {
	  switch (tokenType) {
	  case TokenProvider.ID_TOKEN:
	    return getIDToken();
	  case TokenProvider.ACCESS_TOKEN:
	    return getAccessToken();
	  case TokenProvider.REFRESH_TOKEN:
	    return getRefreshToken();
	  }
    return null;
  }

  @Override
	public long getAccessTokenExpiryTime() {
		return accessTokenExpiryTime;
	}

	@Override
	public void setAccessTokenExpiryTime(long accessTokenExpiryTime) {
		this.accessTokenExpiryTime = accessTokenExpiryTime;
	}

	@Override
	public long getRefreshTokenExpiryTime() {
		return refreshTokenExpiryTime;
	}

	@Override
	public void setRefreshTokenExpiryTime(long refreshTokenExpiryTime) {
		this.refreshTokenExpiryTime = refreshTokenExpiryTime;
	}

	@Override
	public long getLastRefreshedTime() {
		return lastRefreshedTime;
	}

	@Override
	public void setLastRefreshedTime(long lastRefreshTime) {
		this.lastRefreshedTime = lastRefreshTime;
	}

	@Override
	public void save() {
		try {
			ISecurePreferences secureAccountsRoot = AccountModel.getSecureAccountsPreferences();
			Preferences accountsRoot = AccountModel.getAccountsPreferences();
			ISecurePreferences secureAccountNode = secureAccountsRoot.node(server.getId()).node(getId());
			Preferences accountNode = accountsRoot.node(server.getId()).node(getId());

			accountNode.putLong(ACCESS_TOKEN_EXPIRY_TIME_KEY, getAccessTokenExpiryTime());
			accountNode.putLong(REFRESH_TOKEN_EXPIRY_TIME_KEY, getRefreshTokenExpiryTime());
			accountNode.putLong(LAST_REFRESHED_TIME_KEY, getLastRefreshedTime());

			secureAccountNode.put(ID_TOKEN_KEY, getIDToken(), true);
			secureAccountNode.put(ACCESS_TOKEN_KEY, getAccessToken(), true);
			secureAccountNode.put(REFRESH_TOKEN_KEY, getRefreshToken(), true);

			accountNode.flush();
			secureAccountNode.flush();
		} catch (StorageException | BackingStoreException | IOException e) {
			CommonOAuthCoreActivator.logError("Error saving credentials ", e);
		}
	}

	public void load(Preferences accountNode, ISecurePreferences secureAccountNode) throws StorageException {
		accessTokenExpiryTime = accountNode.getLong(ACCESS_TOKEN_EXPIRY_TIME_KEY, Long.MAX_VALUE);
		refreshTokenExpiryTime = accountNode.getLong(REFRESH_TOKEN_EXPIRY_TIME_KEY, Long.MAX_VALUE);
		lastRefreshedTime = accountNode.getLong(LAST_REFRESHED_TIME_KEY, System.currentTimeMillis());
    idToken = secureAccountNode.get(ID_TOKEN_KEY, null);
		accessToken = secureAccountNode.get(ACCESS_TOKEN_KEY, null);
		refreshToken = secureAccountNode.get(REFRESH_TOKEN_KEY, null);
	}
}
