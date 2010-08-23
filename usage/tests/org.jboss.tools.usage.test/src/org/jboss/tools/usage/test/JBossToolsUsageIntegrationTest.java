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

import java.io.UnsupportedEncodingException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.ILoggingAdapter;
import org.jboss.tools.usage.IURLBuildingStrategy;
import org.jboss.tools.usage.PluginLogger;
import org.jboss.tools.usage.Tracker;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageIntegrationTest {

	private static final String HOST_NAME = "www.jboss.org";
	private static final String LOCALE_US = "en_US";

	private IGoogleAnalyticsParameters eclipseEnvironment;

	private static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";

	@Test
	public void sameUserIdOnSametEclipseInstance() throws Exception {
		UrlRevealingTracker tracker = getTracker(getEclipseEnvironmentInstance());
		FocusPoint focusPoint = createFocusPoint("testSameUserIdOnSametEclipseInstance" + System.currentTimeMillis());
		tracker.trackSynchronously(focusPoint);
		String userId = getUserId(tracker.getTrackingUrl());
		assertTrue(userId != null);

		tracker = getTracker(getEclipseEnvironmentInstance());
		tracker.trackSynchronously(focusPoint);
		String newUserId = getUserId(tracker.getTrackingUrl());
		assertTrue(newUserId != null);

		assertEquals(userId, newUserId);
	}

	@Test
	public void differentUserIdOnDifferentEclipseInstance() throws Exception {
		UrlRevealingTracker tracker = getTracker(createEclipseEnvironment());
		tracker.trackSynchronously(createFocusPoint(""));
		String userId = getUserId(tracker.getTrackingUrl());
		assertTrue(userId != null);

		tracker = getTracker(createEclipseEnvironment());
		FocusPoint focusPoint = createFocusPoint("testDifferentUserIdOnDifferentEclipseInstance"
				+ +System.currentTimeMillis());
		tracker.trackSynchronously(focusPoint);
		String newUserId = getUserId(tracker.getTrackingUrl());
		assertTrue(newUserId != null);

		assertTrue(!userId.equals(newUserId));
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
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageTestActivator.getDefault());
		IURLBuildingStrategy urlStrategy = new GoogleAnalyticsUrlStrategy(environment);
		return new UrlRevealingTracker(urlStrategy, environment.getUserAgent(), loggingAdapter);
	}

	private IGoogleAnalyticsParameters getEclipseEnvironmentInstance() {
		if (eclipseEnvironment == null) {
			eclipseEnvironment = createEclipseEnvironment();
		}
		return eclipseEnvironment;
	}

	private IGoogleAnalyticsParameters createEclipseEnvironment() {
		IGoogleAnalyticsParameters eclipseSettings = new EclipseEnvironmentFake(
				GANALYTICS_ACCOUNTNAME
				, HOST_NAME
				, IGoogleAnalyticsParameters.VALUE_NO_REFERRAL
				, Platform.OS_LINUX,
				LOCALE_US);
		return eclipseSettings;
	}

	private FocusPoint createFocusPoint(String childFocusPoint) {
		return new FocusPoint("tools")
					.setChild(new FocusPoint("usage")
							.setChild(new FocusPoint(childFocusPoint)));
	}

	private class UrlRevealingTracker extends Tracker {

		private String trackingUrl;
		private Lock lock;

		public UrlRevealingTracker(IURLBuildingStrategy urlBuildingStrategy, String userAgent,
				ILoggingAdapter loggingAdapter) {
			super(urlBuildingStrategy, userAgent, loggingAdapter);
			lock = new ReentrantLock();
		}

		@Override
		public void trackAsynchronously(FocusPoint focusPoint) {
			lock.lock();
			super.trackAsynchronously(focusPoint);
		}

		@Override
		protected String getTrackingUrl(FocusPoint focusPoint) throws UnsupportedEncodingException {
			trackingUrl = super.getTrackingUrl(focusPoint);
			lock.unlock();
			return trackingUrl;
		}

		private String getTrackingUrl() {
			try {
				lock.lock();
				return trackingUrl;
			} finally {
				lock.unlock();
			}
		}
	}
}
