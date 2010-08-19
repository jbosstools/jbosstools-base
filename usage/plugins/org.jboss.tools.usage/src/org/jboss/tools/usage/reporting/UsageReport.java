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
package org.jboss.tools.usage.reporting;

import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.ILoggingAdapter;
import org.jboss.tools.usage.ITracker;
import org.jboss.tools.usage.IURLBuildingStrategy;
import org.jboss.tools.usage.PluginLogger;
import org.jboss.tools.usage.Tracker;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;

/**
 * @author Andre Dietisheim
 */
public class UsageReport {

	private static final String GANALYTICS_ACCOUNTNAME = ReportingMessages.UsageReport_GoogleAnalyticsAccount;

	private static final String HOST_NAME = ReportingMessages.UsageReport_HostName;

	private FocusPoint focusPoint = new FocusPoint("tools") //$NON-NLS-1$ 
			.setChild(new FocusPoint("usage") //$NON-NLS-1$ 
					.setChild(new FocusPoint("action") //$NON-NLS-1$ 
							.setChild(new FocusPoint("wsstartup") //$NON-NLS-1$ 
							))); //$NON-NLS-1$

	public void report() {
		if (UsageReportPreferences.isEnabled()) {
			report(getAnalyticsTracker());
		}
	}

	private void report(ITracker tracker) {
		tracker.trackAsynchronously(focusPoint);
	}

	private ITracker getAnalyticsTracker() {
		IGoogleAnalyticsParameters eclipseSettings = new EclipseEnvironment(
				GANALYTICS_ACCOUNTNAME
				, HOST_NAME
				, IGoogleAnalyticsParameters.VALUE_NO_REFERRAL);
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageActivator.getDefault());
		IURLBuildingStrategy urlStrategy = new GoogleAnalyticsUrlStrategy(eclipseSettings);
		return new Tracker(urlStrategy, eclipseSettings.getUserAgent(), loggingAdapter);
	}
}
