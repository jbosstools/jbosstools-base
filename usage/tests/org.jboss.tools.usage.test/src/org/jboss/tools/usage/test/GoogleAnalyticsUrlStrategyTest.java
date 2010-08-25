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

import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;

import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class GoogleAnalyticsUrlStrategyTest {

//	private static final String COOKIE_DELIMITER = EncodingUtils.checkedEncodeUtf8(String
//			.valueOf(IGoogleAnalyticsParameters.PLUS_SIGN));
	
	private GoogleAnalyticsUrlStrategy urlStrategy;

	@Before
	public void setUp() {
		this.urlStrategy = new GoogleAnalyticsUrlStrategy(new EclipseEnvironmentFake());
	}

	@Test
	public void testUrlIsCorrect() throws UnsupportedEncodingException {
		FocusPoint focusPoint = new FocusPoint("testing").setChild(new FocusPoint("strategy"));
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
				+ "&utmr=0"
				+ "&utmp=%2Ftesting%2Fstrategy"
				+ "&utmac=UA-17645367-1"
				+ "&utmcc=__utma%3D156030503.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
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

		assertTrue(hasCookieValue("__utma", url));
		assertTrue(hasCookieValue("__utmz", url));
		assertTrue(hasCookieValue("utmcsr", url));
		assertTrue(hasCookieValue("utmccn", url));
		assertTrue(hasCookieValue("utmcmd", url));

		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_GAQ, url, targetUrl));
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
	
//	private void assertEqualCookieParameterValues(String paramName, String url, String targetUrl) {
//		String targetCookieValues = getParameterValue(IGoogleAnalyticsParameters.PARAM_COOKIES, targetUrl,
//				IGoogleAnalyticsParameters.AMPERSAND);
//		String cookieValues = getParameterValue(IGoogleAnalyticsParameters.PARAM_COOKIES, url,
//				IGoogleAnalyticsParameters.AMPERSAND);
//		assertEqualParameterValues(paramName, cookieValues, targetCookieValues, COOKIE_DELIMITER.toCharArray());
//	}

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
