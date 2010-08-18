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
package org.jboss.tools.usage.test;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.ITracker;
import org.jboss.tools.usage.googleanalytics.FocusPoint;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.googleanalytics.ILoggingAdapter;
import org.jboss.tools.usage.googleanalytics.IURLBuildingStrategy;
import org.jboss.tools.usage.googleanalytics.Tracker;
import org.jboss.tools.usage.reporting.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.reporting.PluginLogger;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageTest {

	private static final String HOST_NAME = "jboss.org";
	private static final String LOCALE_US = "en_US";

	private FocusPoint focusPoint = new FocusPoint("jboss.org")
			.setChild(new FocusPoint("tools")
					.setChild(new FocusPoint("usage")
							.setChild(new FocusPoint("test"))));

	private static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";

	@Test
	public void testTrackAsynchronously() throws Exception {
		ITracker tracker = getGoogleAnalyticsTracker();
		tracker.trackAsynchronously(focusPoint);
		Thread.sleep(3000);
	}

	@Test
	public void testTrackSynchronously() throws Exception {
		ITracker tracker = getGoogleAnalyticsTracker();
		tracker.trackSynchronously(focusPoint);
	}

	private ITracker getGoogleAnalyticsTracker() {
		IGoogleAnalyticsParameters eclipseSettings = new EclipseEnvironmentFake(
				GANALYTICS_ACCOUNTNAME, HOST_NAME, JBossToolsUsageTestActivator.PLUGIN_ID, Platform.OS_LINUX, LOCALE_US);
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageTestActivator.getDefault());
		IURLBuildingStrategy urlStrategy = new GoogleAnalyticsUrlStrategy(eclipseSettings);
		return new Tracker(urlStrategy, eclipseSettings.getUserAgent(), loggingAdapter);
	}
}
