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
package org.jboss.tools.usage.googleanalytics;

import java.io.UnsupportedEncodingException;

import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.IURLBuildingStrategy;
import org.jboss.tools.usage.util.HttpEncodingUtils;

/**
 * Class that builds an URL that passes given parameters to google analytics
 * 
 * @author Andre Dietisheim
 * 
 * @see <a
 *      href="http://code.google.com/apis/analytics/docs/tracking/gaTrackingTroubleshooting.html#gifParameters">GIF
 *      Request Parameters</a>
 * @see <a
 *      href="http://code.google.com/apis/analytics/docs/concepts/gaConceptsCookies.html#cookiesSet">Cookies
 *      Set By Google Analytics</a>
 * 
 * @see <a
 *      href="http://www.morevisibility.com/analyticsblog/from-__utma-to-__utmz-google-analytics-cookies.html">From
 *      __utma to __utmz (Google Analytics Cookies)</a>
 */
public class GoogleAnalyticsUrlStrategy implements IURLBuildingStrategy {

	private static final String TRACKING_URL = "http://www.google-analytics.com/__utm.gif";

	// private static final int VISITS = -1;

	private IGoogleAnalyticsParameters googleParameters;

	public GoogleAnalyticsUrlStrategy(IGoogleAnalyticsParameters googleAnalyticsParameters) {
		this.googleParameters = googleAnalyticsParameters;
	}

	public String build(FocusPoint focusPoint) throws UnsupportedEncodingException {
		/*
		 * Google Analytics for Android:
		 * 
		String str = ""; 
		if (paramEvent.action != null) 
			str = paramEvent.action; 
		if (!(str.startsWith("/"))) 
			str = "/" + str; 
		str = encode(str); 
		Locale localLocale = Locale.getDefault(); 
		StringBuilder localStringBuilder = new StringBuilder();
		localStringBuilder.append("/__utm.gif");
		localStringBuilder.append("?utmwv=4.3");
		localStringBuilder.append("&utmn=").append(paramEvent.randomVal);
		localStringBuilder.append("&utmcs=UTF-8");
		localStringBuilder.append(String.format("&utmsr=%dx%d", new Object[] { 
			Integer.valueOf(paramEvent.screenWidth)
			, Integer.valueOf(paramEvent.screenHeight) }));
		localStringBuilder.append(String.format("&utmul=%s-%s", new Object[] { 
			localLocale.getLanguage()
			, localLocale.getCountry() }));
		localStringBuilder.append("&utmp=").append(str);
		localStringBuilder.append("&utmac=").append(paramEvent.accountId);
		localStringBuilder.append("&utmcc=").append(
		  		getEscapedCookieString(paramEvent, paramString)); 
		return localStringBuilder.toString();
		 
		*
		* getEscapedCookieString:
		* 
		StringBuilder localStringBuilder = new StringBuilder();
		localStringBuilder.append("__utma=");
		localStringBuilder.append("999").append(".");
		localStringBuilder.append(paramEvent.userId).append(".");
		localStringBuilder.append(paramEvent.timestampFirst).append(".");
		localStringBuilder.append(paramEvent.timestampPrevious).append(".");
		localStringBuilder.append(paramEvent.timestampCurrent).append(".");
		localStringBuilder.append(paramEvent.visits);
		if (paramString != null)
		{
		localStringBuilder.append("+__utmz=");
		localStringBuilder.append("999").append(".");
		localStringBuilder.append(paramEvent.timestampFirst).append(".");
		localStringBuilder.append("1.1.");
		localStringBuilder.append(paramString);
		}
		return encode(localStringBuilder.toString());

		 */

		StringBuilder builder = new StringBuilder(TRACKING_URL)
				.append(IGoogleAnalyticsParameters.URL_PARAM_DELIMITER);
		appendParameter(IGoogleAnalyticsParameters.PARAM_TRACKING_CODE_VERSION,
				IGoogleAnalyticsParameters.VALUE_TRACKING_CODE_VERSION, builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_UNIQUE_TRACKING_NUMBER, getRandomNumber(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_HOST_NAME, googleParameters.getHostname(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_LANGUAGE_ENCODING,
				IGoogleAnalyticsParameters.VALUE_ENCODING_UTF8, builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_SCREEN_RESOLUTION, googleParameters.getScreenResolution(),
				builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_SCREEN_COLOR_DEPTH, googleParameters.getScreenColorDepth(),
				builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_BROWSER_LANGUAGE, googleParameters.getBrowserLanguage(),
				builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_PAGE_TITLE, focusPoint.getContentTitle(), builder);
		// appendParameter(IGoogleAnalyticsParameters.PARAM_HID,
		// getRandomNumber(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_REFERRAL, googleParameters.getReferral(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_PAGE_REQUEST, focusPoint.getContentURI(), builder);

		appendParameter(IGoogleAnalyticsParameters.PARAM_ACCOUNT_NAME, googleParameters.getAccountName(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_COOKIES, getCookies(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_GAQ, "1", false, builder);

		googleParameters.visit();

		return builder.toString();
	}

	/**
	 * Returns the google analytics cookies. These cookies determines user
	 * identity, session identity etc.
	 * 
	 * @return the cookies
	 * 
	 * @see <a
	 *      href="http://www.analyticsexperts.com/google-analytics/information-about-the-utmlinker-and-the-__utma-__utmb-and-__utmc-cookies/">Information
	 *      about the utmLinker and the __utma, __utmb and __utmc cookies</a>
	 */
	private String getCookies() {
		StringBuilder builder = new StringBuilder();

		/**
		 * unique visitor id cookie has to be unique per eclipse installation
		 */
		// String timeStamp = "-1";
		// //String.valueOf(System.currentTimeMillis());
		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_UNIQUE_VISITOR_ID,
				new StringBuilder().append("999.")
						.append(googleParameters.getUserId()).append(IGoogleAnalyticsParameters.DOT)
						.append(googleParameters.getFirstVisit()).append(IGoogleAnalyticsParameters.DOT)
						.append(googleParameters.getLastVisit()).append(IGoogleAnalyticsParameters.DOT)
						.append(googleParameters.getCurrentVisit()).append(IGoogleAnalyticsParameters.DOT)
						.append(googleParameters.getVisitCount())
						.append(IGoogleAnalyticsParameters.SEMICOLON),
				IGoogleAnalyticsParameters.PLUS_SIGN)
				.appendTo(builder);

		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_REFERRAL_TYPE,
						new StringBuilder()
								.append("999.")
								.append(googleParameters.getFirstVisit())
								.append(IGoogleAnalyticsParameters.DOT)
								.append("1.1."))
				.appendTo(builder);

//		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_SESSION,
//				new StringBuilder()
//						.append("1"),
//						IGoogleAnalyticsParameters.SEMICOLON
//						, IGoogleAnalyticsParameters.PLUS_SIGN)
//				.appendTo(builder);
//
//		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_BROWSERSESSION,
//				new StringBuilder()
//						.append("1"),
//						IGoogleAnalyticsParameters.SEMICOLON
//						, IGoogleAnalyticsParameters.PLUS_SIGN)
//				.appendTo(builder);

		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_UTMCSR,
						"(direct)",
						IGoogleAnalyticsParameters.PIPE)
				.appendTo(builder);

		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_UTMCCN,
						"(direct)",
						IGoogleAnalyticsParameters.PIPE)
				.appendTo(builder);

		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_UTMCMD,
						"(none)",
						IGoogleAnalyticsParameters.PIPE)
				.appendTo(builder);

		new GoogleAnalyticsCookie(IGoogleAnalyticsParameters.PARAM_COOKIES_KEYWORD,
					googleParameters.getKeyword())
				.appendTo(builder);

		builder.append(IGoogleAnalyticsParameters.SEMICOLON);

		return HttpEncodingUtils.checkedEncodeUtf8(builder.toString());
	}

	private String getRandomNumber() {
		return Integer.toString((int) (Math.random() * 0x7fffffff));
	}

	private void appendParameter(String name, String value, StringBuilder builder) {
		appendParameter(name, value, true, builder);
	}

	private void appendParameter(String name, String value, boolean appendAmpersand, StringBuilder builder) {
		builder.append(name)
				.append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append(value);
		if (appendAmpersand) {
			builder.append(IGoogleAnalyticsParameters.AMPERSAND);
		}
	}
}
