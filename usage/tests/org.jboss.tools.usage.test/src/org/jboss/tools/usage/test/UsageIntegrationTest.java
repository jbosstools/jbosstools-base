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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.http.HttpGetRequest;
import org.jboss.tools.usage.http.IHttpGetRequest;
import org.jboss.tools.usage.test.fakes.ReportingEclipseEnvironmentFake;
import org.jboss.tools.usage.test.fakes.RepportingEclipseEnvironmentFakeSingleton;
import org.jboss.tools.usage.tracker.IFocusPoint;
import org.jboss.tools.usage.tracker.IURLBuildingStrategy;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class UsageIntegrationTest {

	@Test
	public void sameUserIdOnSametEclipseInstance() throws Exception {
		UrlRevealingTracker tracker = getTracker(RepportingEclipseEnvironmentFakeSingleton.INSTANCE);
		IFocusPoint focusPoint = createFocusPoint("testSameUserIdOnSametEclipseInstance" + System.currentTimeMillis());
		tracker.trackSynchronously(focusPoint);
		String userId = getUserId(tracker.getTrackingUrl());
		assertTrue(userId != null);

		tracker = getTracker(RepportingEclipseEnvironmentFakeSingleton.INSTANCE);
		tracker.trackSynchronously(focusPoint);
		String newUserId = getUserId(tracker.getTrackingUrl());

		assertTrue(newUserId != null);
		assertEquals(userId, newUserId);
	}

	@Test
	public void differentUserIdOnDifferentEclipseInstance() throws Exception {
		String focusPointName = "testDifferentUserIdOnDifferentEclipseInstance"
				+ System.currentTimeMillis();
		UrlRevealingTracker tracker = getTracker(new ReportingEclipseEnvironmentFake());
		tracker.trackSynchronously(createFocusPoint(focusPointName));
		String userId = getUserId(tracker.getTrackingUrl());
		assertTrue(userId != null);

		tracker = getTracker(new ReportingEclipseEnvironmentFake());
		IFocusPoint focusPoint = createFocusPoint(focusPointName);
		tracker.trackSynchronously(focusPoint);
		String newUserId = getUserId(tracker.getTrackingUrl());

		assertTrue(newUserId != null);
		assertTrue(!userId.equals(newUserId));
	}

	@Test
	public void visitCountIncreases() throws Exception {
		IGoogleAnalyticsParameters eclipseEnvironment = new ReportingEclipseEnvironmentFake();
		assertEquals(1, eclipseEnvironment.getVisitCount());
		UrlRevealingTracker tracker = getTracker(eclipseEnvironment);
		tracker.trackSynchronously(createFocusPoint("testVisitCount"));
		assertEquals(2, eclipseEnvironment.getVisitCount());
		tracker.trackSynchronously(createFocusPoint("testVisitCount"));
		assertEquals(3, eclipseEnvironment.getVisitCount());
	}

	@Test
	public void urlEndsWithJBossToolsVersion() {
		IGoogleAnalyticsParameters eclipseEnvironment = new ReportingEclipseEnvironmentFake();
		UrlRevealingTracker tracker = getTracker(eclipseEnvironment);
		tracker.trackSynchronously(createFocusPoint("testJBossToolsVersion"));
		assertEquals(2, eclipseEnvironment.getVisitCount());
		tracker.trackSynchronously(createFocusPoint("testVisitCount"));
		assertEquals(3, eclipseEnvironment.getVisitCount());
	}

	private String getUserId(String trackingUrl) {
		Pattern pattern = Pattern.compile(".+" + IGoogleAnalyticsParameters.PARAM_COOKIES_UNIQUE_VISITOR_ID
				+ "%3D([0-9]+\\.[0-9]+)\\..+");
		Matcher matcher = pattern.matcher(trackingUrl);
		if (matcher.matches()) {
			return matcher.group(1);
		} else {
			return null;
		}
	}

	private UrlRevealingTracker getTracker(IGoogleAnalyticsParameters environment) {
		UsagePluginLogger logger = new UsagePluginLogger(JBossToolsUsageTestActivator.getDefault());
		IURLBuildingStrategy urlStrategy = new GoogleAnalyticsUrlStrategy(environment);
		IHttpGetRequest httpGetRequest = new HttpGetRequest(environment.getUserAgent(), logger);
		return new UrlRevealingTracker(urlStrategy, httpGetRequest, logger);
	}

	private IFocusPoint createFocusPoint(String childFocusPoint) {
		return new JBossToolsTestsFocusPoint(childFocusPoint);
	}
}
