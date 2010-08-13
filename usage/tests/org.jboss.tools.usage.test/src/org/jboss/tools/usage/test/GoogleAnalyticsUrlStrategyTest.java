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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;

import org.jboss.tools.usage.jgoogleanalytics.EclipseEnvironment;
import org.jboss.tools.usage.jgoogleanalytics.FocusPoint;
import org.jboss.tools.usage.jgoogleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.jgoogleanalytics.IGoogleAnalyticsParameters;
import org.junit.Before;
import org.junit.Test;

public class GoogleAnalyticsUrlStrategyTest {

	private static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";
	private static final String HOSTNAME = "jboss.org";
	private GoogleAnalyticsUrlStrategy urlStrategy;

	@Before
	public void setUp() {
		this.urlStrategy = new EclipseEnvironmentGAUrlStrategy();
	}

	@Test
	public void testUrlIsBuiltCorrectly() throws UnsupportedEncodingException {
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
				+ "&__utma%3D156030503.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
				+ "&gaq=1";
		// assertEquals(expectedUrl, builtUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_TRACKING_CODE_VERSION, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_HOST_NAME, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_LANGUAGE_ENCODING, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_SCREEN_RESOLUTION, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_SCREEN_COLOR_DEPTH, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_PAGE_TITLE, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_REFERRAL, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_PAGE_REQUEST, url, targetUrl);
		assertEqualParameterValues(IGoogleAnalyticsParameters.PARAM_ACCOUNT_NAME, url, targetUrl);
	}

	private void assertEqualParameterValues(String paramName, String url, String targetUrl) {
		String targetValue = getParameterValue(paramName, targetUrl,IGoogleAnalyticsParameters.AMPERSAND);
		String value = getParameterValue(paramName, url,IGoogleAnalyticsParameters.AMPERSAND);
		assertEquals("parameter '" + paramName + "' did not match", targetValue, value);
	}

	private String getParameterValue(String parameterName, String url, char... delimiters) {
		String value = null;
		int parameterNameStart = url.indexOf(parameterName);
		if (parameterNameStart > 0) {
			for (char delimiter : delimiters) {
				int valueEnd = url.indexOf(delimiter, parameterNameStart + parameterName.length());
				if (valueEnd > 0) {
					value = url.substring(parameterNameStart + parameterName.length() + 1, valueEnd);
					break;
				}
			}
		}
		return value;
	}

	private class EclipseEnvironmentGAUrlStrategy extends GoogleAnalyticsUrlStrategy {
		private EclipseEnvironmentGAUrlStrategy() {
			super(new EclipseEnvironmentFake());
		}
	}

	private class EclipseEnvironmentFake extends EclipseEnvironment {

		public EclipseEnvironmentFake() {
			super(GANALYTICS_ACCOUNTNAME, HOSTNAME, "0");
		}

		@Override
		protected void initScreenSettings() {
			// do not access swt/display
		}

		@Override
		public String getScreenResolution() {
			return 1920 + SCREERESOLUTION_DELIMITER + 1080;
		}

		@Override
		public String getScreenColorDepth() {
			return 24 + SCREENCOLORDEPTH_POSTFIX;
		}

	}
}
