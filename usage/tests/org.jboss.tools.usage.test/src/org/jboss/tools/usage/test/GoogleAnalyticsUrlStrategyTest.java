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

import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.test.fakes.ReportingEclipseEnvironmentFake;
import org.jboss.tools.usage.tracker.internal.FocusPoint;
import org.jboss.tools.usage.tracker.internal.IFocusPoint;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class GoogleAnalyticsUrlStrategyTest {

	private GoogleAnalyticsUrlStrategy urlStrategy;

	@Before
	public void setUp() {
		this.urlStrategy = new GoogleAnalyticsUrlStrategy(new ReportingEclipseEnvironmentFake());
	}

	@Test
	public void createsCorrectUrl() throws UnsupportedEncodingException {
		IFocusPoint focusPoint = new FocusPoint("testing").setChild(new FocusPoint("strategy"));
		String url = urlStrategy.build(focusPoint);
		String targetUrl = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=33832126513"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=en-us"
				+ "&utmdt=testing-strategy"
				+ "&utmhid=1087431432"
				+ "&utmr="
					+ IGoogleAnalyticsParameters.VALUE_NO_REFERRAL
				+ "&utmp=%2Ftesting%2Fstrategy"
				+ "&utmfl="
					+ ReportingEclipseEnvironmentFake.JAVA_VERSION
				+ "&utmac=UA-17645367-1"
				+ "&utmcc=__utma%3D156030503.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
				+ "&utm_content=" + JBossToolsUsageActivator.getDefault().getBundle().getVersion()
				+ "&gaq=1";
		
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_TRACKING_CODE_VERSION, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_HOST_NAME, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_LANGUAGE_ENCODING, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_SCREEN_RESOLUTION, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_SCREEN_COLOR_DEPTH, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_PAGE_TITLE, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_REFERRAL, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_PAGE_REQUEST, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_ACCOUNT_NAME, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_FLASH_VERSION, url, targetUrl));
		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_AD_CONTENT, url, targetUrl));

		assertTrue(hasCookieValue("__utma", url));
		assertTrue(hasCookieValue("__utmz", url));
		assertTrue(hasCookieValue("utmcsr", url));
		assertTrue(hasCookieValue("utmccn", url));
		assertTrue(hasCookieValue("utmcmd", url));

		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_GAQ, url, targetUrl));
	}

	@Test
	public void visitCountIncreases() throws Exception {
		IGoogleAnalyticsParameters eclipseEnvironment = new ReportingEclipseEnvironmentFake();
		assertEquals(1, eclipseEnvironment.getVisitCount());
		eclipseEnvironment.visit();
		assertEquals(2, eclipseEnvironment.getVisitCount());
		eclipseEnvironment.visit();
		assertEquals(3, eclipseEnvironment.getVisitCount());
	}
	
	private boolean areEqualParameterValues(String paramName, String url, String targetUrl) {
		return areEqualParameterValues(paramName, url, targetUrl, String.valueOf(IGoogleAnalyticsParameters.AMPERSAND));
	}

	private boolean areEqualParameterValues(String paramName, String url, String targetUrl, String delimiters) {
		String targetValue = getParameterValue(paramName, targetUrl, delimiters);
		String value = getParameterValue(paramName, url, delimiters);
		return targetValue != null && targetValue.equals(value);
	}

	private boolean hasCookieValue(String cookieName, String url) {
		String cookieValues = getParameterValue(IGoogleAnalyticsParameters.PARAM_COOKIES, url,
				String.valueOf(IGoogleAnalyticsParameters.AMPERSAND));
		return cookieValues != null && cookieValues.indexOf(cookieName) >= 0;
	}

	private String getParameterValue(String parameterName, String url, String delimiters) {
		String value = null;
		int parameterNameStart = url.indexOf(parameterName);
		if (parameterNameStart >= 0) {
			int valueStart = parameterNameStart + parameterName.length() + 1;
			int valueEnd = url.indexOf(delimiters, parameterNameStart + parameterName.length());
			if (valueEnd < 0) {
				value = url.substring(valueStart);
			} else {
				value = url.substring(valueStart, valueEnd);
			}
		}
		return value;
	}
}
