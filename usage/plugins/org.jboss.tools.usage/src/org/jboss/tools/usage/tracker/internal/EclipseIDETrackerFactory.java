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

package org.jboss.tools.usage.tracker.internal;

import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.http.HttpGetRequest;
import org.jboss.tools.usage.internal.JBDSUtils;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.preferences.UsageReportPreferencesUtils;
import org.jboss.tools.usage.reporting.ReportingEclipseEnvironment;
import org.jboss.tools.usage.reporting.ReportingMessages;
import org.jboss.tools.usage.tracker.ILoggingAdapter;
import org.jboss.tools.usage.tracker.ITracker;
import org.jboss.tools.usage.tracker.IURLBuildingStrategy;

public class EclipseIDETrackerFactory {
	
	public static final EclipseIDETrackerFactory INSTANCE = new EclipseIDETrackerFactory();
	
	public ITracker create() {
		IGoogleAnalyticsParameters eclipseEnvironment = createEclipseEnvironment();
		IURLBuildingStrategy urlBuildingStrategy = new GoogleAnalyticsUrlStrategy(eclipseEnvironment);
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageActivator.getDefault());
		return new Tracker(
				urlBuildingStrategy
				, new HttpGetRequest(eclipseEnvironment.getUserAgent(), loggingAdapter)
				, loggingAdapter);
	}

	private IGoogleAnalyticsParameters createEclipseEnvironment() {
		return new ReportingEclipseEnvironment(getGoogleAnalyticsAccount(), getGoogleAnalyticsHostname(), UsageReportPreferencesUtils.getPreferences());
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
