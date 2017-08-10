/*******************************************************************************
 * Copyright (c) 2010-2017 Red Hat, Inc.
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

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.internal.environment.IUsageEnvironment;
import org.jboss.tools.usage.test.fakes.BundleGroupProviderFake;
import org.jboss.tools.usage.test.fakes.EclipsePreferencesFake;
import org.jboss.tools.usage.test.fakes.EclipseUserAgentFake;
import org.jboss.tools.usage.test.fakes.ReportingEclipseEnvironmentFake;
import org.jboss.tools.usage.tracker.IFocusPoint;
import org.jboss.tools.usage.tracker.internal.FocusPoint;
import org.jboss.tools.usage.util.HttpEncodingUtils;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class GoogleAnalyticsUrlStrategyTest {

	private static final String URLENCODED_SEMICOLON =
			HttpEncodingUtils.checkedEncodeUtf8(String.valueOf(IGoogleAnalyticsParameters.SEMICOLON));
	private static final String URLENCODED_EQUALS_SIGN =
			HttpEncodingUtils.checkedEncodeUtf8(String.valueOf(IGoogleAnalyticsParameters.EQUALS_SIGN));
	private GoogleAnalyticsUrlStrategy urlStrategy;

	@Before
	public void setUp() {
		this.urlStrategy = new GoogleAnalyticsUrlStrategy(
				new ReportingEclipseEnvironmentFake(
						JBossToolsTestBranding.GOOGLE_ANALYTICS_TEST_ACCOUNT,
						JBossToolsTestBranding.REPORTING_HOST,
						ReportingEclipseEnvironmentFake.JAVA_VERSION,
						new EclipsePreferencesFake(),
						new EclipseUserAgentFake()) {

					protected IBundleGroupProvider[] getBundleGroupProviders() {
						return new IBundleGroupProvider[] {
								new BundleGroupProviderFake(
										"org.jboss.tools.gwt.feature",
										"org.jboss.tools.seam.feature",
										"org.jboss.tools.smooks.feature")
						};
					}
				});
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
				+ "&utmac="
				+ JBossToolsTestBranding.GOOGLE_ANALYTICS_TEST_ACCOUNT
				+ "&utmcc=__utma%3D156030503.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
				+ "__utmv=404606403.Fedora+13"
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

		assertTrue(hasCookieValue("__utma", url));
		assertTrue(hasCookieValue("__utmz", url));
		assertTrue(hasCookieValue("utmcsr", url));
		assertTrue(hasCookieValue("utmccn", url));
		assertTrue(hasCookieValue("utmcmd", url));
		assertEquals("GWT-SEAM-SMOOKS", getCookieValue("utmctr", url));
		assertTrue(getCookieValue("__utmv", url).contains(HttpEncodingUtils.checkedEncodeUtf8("Fedora 13")));

		assertTrue(areEqualParameterValues(IGoogleAnalyticsParameters.PARAM_GAQ, url, targetUrl));
	}

	@Test
	public void visitCountIncreases() throws Exception {
		IUsageEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake();
		assertEquals(1, eclipseEnvironment.getVisitCount());
		eclipseEnvironment.visit();
		assertEquals(2, eclipseEnvironment.getVisitCount());
		eclipseEnvironment.visit();
		assertEquals(3, eclipseEnvironment.getVisitCount());
	}

	@Test
	public void verifyCentralIsStarted() throws IOException {
		IFocusPoint focusPoint = new FocusPoint("testing").setChild(new FocusPoint("strategy"));
		String url = urlStrategy.build(focusPoint);

		String centralEnabled = new ReportingEclipseEnvironmentFake().getCentralEnabledValue();

		assertTrue(areEqualParameterValues(
				IGoogleAnalyticsParameters.PARAM_EVENT_TRACKING
				, url
				, IGoogleAnalyticsParameters.PARAM_EVENT_TRACKING + "=5(central*showOnStartup*" + centralEnabled + ")&"));
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
		return getCookieValue(cookieName, url) != null;
	}

	private String getCookieValue(String cookieName, String url) {
		String cookieValues = getParameterValue(IGoogleAnalyticsParameters.PARAM_COOKIES, url,
				String.valueOf(IGoogleAnalyticsParameters.AMPERSAND));
		if (cookieValues == null) {
			return null;
		}
		int cookieNameStart = cookieValues.indexOf(cookieName);
		if (cookieNameStart < 0) {
			return null;
		}
		int cookieNameStop = cookieValues.substring(cookieNameStart)
				.indexOf(URLENCODED_EQUALS_SIGN);
		if (cookieNameStop < 0) {
			return null;
		}
		int cookieValueStart = cookieNameStart + cookieNameStop + URLENCODED_EQUALS_SIGN.length();
		// cookie must be terminated by ';'
		int cookieValueStop =
				cookieValues.substring(cookieValueStart).indexOf(URLENCODED_SEMICOLON);
		if (cookieValueStop < 0) {
			return null;
		}
		return cookieValues.substring(cookieValueStart, cookieValueStart + cookieValueStop);
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
