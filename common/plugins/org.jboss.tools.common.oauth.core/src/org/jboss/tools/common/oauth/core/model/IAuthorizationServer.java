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
package org.jboss.tools.common.oauth.core.model;

import java.util.List;

public interface IAuthorizationServer {
	String getId();
	
	String getRealm();

	void setRealm(String reaml);

	String getURL();
	
	void setURL(String url);

	String getClientId();

	void setClientId(String clientId);
	
	String getDisplayName();

	void addAccount(IAccount account);

	List<IAccount> getAccounts();

	void removeAccount(IAccount account);

	IAccount createAccount(String id);

	void save();
}
