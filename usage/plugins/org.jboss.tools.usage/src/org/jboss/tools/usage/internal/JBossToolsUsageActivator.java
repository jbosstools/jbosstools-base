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

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferencesUtils;
import org.jboss.tools.usage.internal.reporting.JBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.reporting.ReportingMessages;
import org.osgi.framework.BundleContext;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageActivator extends Plugin {

	public static final String PLUGIN_ID = "org.jboss.tools.usage"; //$NON-NLS-1$

	private static JBossToolsUsageActivator plugin;

	private IJBossToolsEclipseEnvironment eclipseEnvironment;
	
	public JBossToolsUsageActivator() {
		plugin = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	public static JBossToolsUsageActivator getDefault() {
		return plugin;
	}
	
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	public IJBossToolsEclipseEnvironment getJBossToolsEclipseEnvironment() {
		if (eclipseEnvironment == null) {
			eclipseEnvironment = createEclipseEnvironment();
		}
		return eclipseEnvironment;
	}

	private IJBossToolsEclipseEnvironment createEclipseEnvironment() {
		return new JBossToolsEclipseEnvironment(
				getGoogleAnalyticsAccount(), getGoogleAnalyticsHostname(),
				UsageReportPreferencesUtils.getPreferences());
	}

	private String getGoogleAnalyticsAccount() {
		if (JBDSUtils.isJBDS()) {
			return ReportingMessages.UsageReport_GoogleAnalytics_Account_JBDS;
		} else {
			return ReportingMessages.UsageReport_GoogleAnalytics_Account;
		}
	}

	private String getGoogleAnalyticsHostname() {
		if (JBDSUtils.isJBDS()) {
			return ReportingMessages.UsageReport_HostName_JBDS;
		} else {
			return ReportingMessages.UsageReport_HostName;
		}
	}
}
