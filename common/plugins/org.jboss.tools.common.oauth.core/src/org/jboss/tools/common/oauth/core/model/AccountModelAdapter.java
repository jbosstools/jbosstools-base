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

public class AccountModelAdapter implements IAccountModelListener {

	@Override
	public void accountAdded(IAccountModel source, IAccount account) {
	}

	@Override
	public void accountRemoved(IAccountModel source, IAccount account) {
	}

	@Override
	public void authorizationServerAdded(IAccountModel source, IAuthorizationServer server) {
	}

	@Override
	public void authorizationRemoved(IAccountModel source, IAuthorizationServer server) {
	}
}
