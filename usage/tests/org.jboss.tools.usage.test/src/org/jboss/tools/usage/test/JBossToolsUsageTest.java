/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import org.jboss.tools.usage.IUsageTracker;
import org.jboss.tools.usage.jgoogleanalytics.EclipseEnvironment;
import org.jboss.tools.usage.jgoogleanalytics.FocusPoint;
import org.jboss.tools.usage.jgoogleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.jgoogleanalytics.Tracker;
import org.jboss.tools.usage.jgoogleanalytics.PluginLogger;
import org.jboss.tools.usage.test.internal.JBossToolsUsageTestActivator;
import org.junit.Test;

public class JBossToolsUsageTest {

	private static final String HOST_NAME = "jboss.org";
	
	private FocusPoint focusPoint = new FocusPoint("jboss.org")
			.setChild(new FocusPoint("tools")
					.setChild(new FocusPoint("usage")
							.setChild(new FocusPoint("instance"))));

	private static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";

	@Test
	public void testTrackAsynchronously() throws Exception {
		IUsageTracker tracker = getGoogleAnalyticsTracker();
		tracker.trackAsynchronously(focusPoint);
		Thread.sleep(3000);
	}

	@Test
	public void testTrackSynchronously() throws Exception {
		IUsageTracker tracker = getGoogleAnalyticsTracker();
		tracker.trackSynchronously(focusPoint);
	}

	private IUsageTracker getGoogleAnalyticsTracker() {
		IGoogleAnalyticsParameters eclipseSettings = new EclipseEnvironment(
				GANALYTICS_ACCOUNTNAME, HOST_NAME, JBossToolsUsageTestActivator.PLUGIN_ID);
		Tracker tracker = new Tracker(eclipseSettings);
		tracker.setLoggingAdapter(new PluginLogger(JBossToolsUsageTestActivator.getDefault()));
		return tracker;
	}
}
