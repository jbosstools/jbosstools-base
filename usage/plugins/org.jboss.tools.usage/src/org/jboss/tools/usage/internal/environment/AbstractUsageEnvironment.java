/*******************************************************************************
 * Copyright (c) 2010-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.environment;

/**
 * @author Andre Dietisheim
 */
public abstract class AbstractUsageEnvironment implements IUsageEnvironment {

	private String accountName;
	private String hostName;
	private String userDefined;

	public AbstractUsageEnvironment(String accountName, String hostName) {
		this(accountName, hostName, null);
	}

	public AbstractUsageEnvironment(String accountName, String hostName, String userDefined) {
		this.accountName = accountName;
		this.hostName = hostName;
		this.userDefined = userDefined;
	}

	@Override
	public String getAccountName() {
		return accountName;
	}

	@Override
	public String getHostname() {
		return hostName;
	}
	
	@Override
	public String getUserDefined() {
		return userDefined;
	}
}
