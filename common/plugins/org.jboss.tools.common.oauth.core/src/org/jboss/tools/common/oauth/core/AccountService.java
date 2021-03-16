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
package org.jboss.tools.common.oauth.core;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import org.jboss.tools.common.oauth.core.exception.OAuthConfigurationException;
import org.jboss.tools.common.oauth.core.exception.OAuthLoginException;
import org.jboss.tools.common.oauth.core.exception.OAuthRefreshException;
import org.jboss.tools.common.oauth.core.model.IAccount;
import org.jboss.tools.common.oauth.core.model.IAccountModel;
import org.jboss.tools.common.oauth.core.model.IAuthorizationServer;
import org.jboss.tools.common.oauth.internal.core.model.AccountModel;
import org.keycloak.adapters.KeycloakDeployment;
import org.keycloak.adapters.ServerRequest;
import org.keycloak.adapters.ServerRequest.HttpFailure;
import org.keycloak.representations.AccessTokenResponse;

public class AccountService {

	private static final AccountService INSTANCE = new AccountService();

	private final LoginProvider provider = LoginProvider.get();

	private IAccountModel model;

	private AccountService() {
	}

	public static AccountService getDefault() {
		return INSTANCE;
	}

	public IAccountModel getModel() {
		if (null == model) {
			model = new AccountModel();
		}
		return model;
	}

	public AccountStatus getStatus(IAccount account) {
		if (account.getAccessToken() == null) {
			return AccountStatus.NEEDS_LOGIN;
		}
		long lastRefreshed = account.getLastRefreshedTime();
		long current = System.currentTimeMillis();
		if (current > account.getAccessTokenExpiryTime()) {
			if (current > account.getRefreshTokenExpiryTime()) {
				return AccountStatus.NEEDS_LOGIN;
			} else {
				return AccountStatus.NEEDS_REFRESH;
			}
		}
		if (wasRefreshed24HAgo(lastRefreshed, current) || wasRefreshedMoreThanHalfTheTotalValidPeriod(
				account.getAccessTokenExpiryTime(), lastRefreshed, current)) {
			return AccountStatus.NEEDS_REFRESH;
		}
		return AccountStatus.VALID;
	}

	boolean wasRefreshedMoreThanHalfTheTotalValidPeriod(long expiryTime, long lastRefreshed, long current) {
		return (current - lastRefreshed) > (expiryTime - current);
	}

	boolean wasRefreshed24HAgo(long lastRefreshed, long current) {
		return (current - lastRefreshed) > CommonOAuthCoreConstants.DURATION_24_HOURS;
	}
	
	private IAuthorizationServer findAuthorizationServer(String serverId) {
	  Optional<IAuthorizationServer> server = getModel().getAuthorizationServers().stream().filter(cl -> serverId.equals(cl.getId())).findFirst();
	  return server.orElseGet(null);
	}

	public String getToken(String serverId, Object context) {
		String token = null;

		IAuthorizationServer server = findAuthorizationServer(serverId);
		List<IAccount> identities = server.getAccounts();
		if (identities.isEmpty()) {
			token = performLogin(server, null, context);
		} else {
			IAccount account = identities.get(0);
			AccountStatus status = getStatus(account);
			switch (status) {
			case VALID:
				token = account.getAccessToken();
				break;
			case NEEDS_REFRESH:
				token = performRefresh(account);
				break;
			case NEEDS_LOGIN:
				token = performLogin(server, account, context);
				break;
			}

		}
		return token;
	}

	private String performLogin(IAuthorizationServer server, IAccount account, Object context) {
		if (null != provider) {
			LoginResponse response = provider.login(server, account, context);
			if (null != response) {
				if (null == account) {
					IAccount newAccount = createAccount(server, response);
					return newAccount.getAccessToken();
				} else {
					updateAccount(response, account);
				}
				return account.getAccessToken();
			} else {
				throw new OAuthLoginException(server, account);
			}
		} else {
			throw new OAuthConfigurationException("No login provider found");
		}
	}

	IAccount createAccount(IAuthorizationServer server, LoginResponse response) {
		String id = OAuthUtils.decodeEmailFromToken(response.getAccessToken());
		IAccount newAccount = server.createAccount(id);
		updateAccount(response, newAccount);
		server.addAccount(newAccount);
		return newAccount;
	}
	
	void updateAccount(LoginResponse info, IAccount account) {
		account.setAccessToken(info.getAccessToken());
		account.setRefreshToken(info.getRefreshToken());
		account.setLastRefreshedTime(System.currentTimeMillis());
		account.setAccessTokenExpiryTime(info.getAccessTokenExpiryTime());
		account.setRefreshTokenExpiryTime(info.getRefreshTokenExpiryTime());
		account.save();
	}

	private String performRefresh(IAccount account) {
	  try {
      KeycloakDeployment deployment = OAuthUtils.getDeployment(account.getAuthorizationServer());
      AccessTokenResponse response = ServerRequest.invokeRefresh(deployment, account.getRefreshToken());
      account.setAccessToken(response.getIdToken());
      account.setRefreshToken(response.getRefreshToken());
      account.setAccessTokenExpiryTime(System.currentTimeMillis() + response.getExpiresIn());
      account.setRefreshTokenExpiryTime(System.currentTimeMillis() + response.getRefreshExpiresIn());
      account.setLastRefreshedTime(System.currentTimeMillis());
      account.save();
      return account.getAccessToken();
    } catch (IOException | HttpFailure e) {
      throw new OAuthRefreshException(account, e);
    }
	}
}
