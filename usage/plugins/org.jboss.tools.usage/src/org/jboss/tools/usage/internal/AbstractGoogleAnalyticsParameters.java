/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;

/**
 * @author Andre Dietisheim
 */
public abstract class AbstractGoogleAnalyticsParameters implements IGoogleAnalyticsParameters {

	private String accountName;
	private String referral;
	private String hostName;

	public AbstractGoogleAnalyticsParameters(String accountName, String hostName, String referral) {
		this.accountName = accountName;
		this.hostName = hostName;
		this.referral = referral;
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
	public String getReferral() {
		return referral;
	}
}
