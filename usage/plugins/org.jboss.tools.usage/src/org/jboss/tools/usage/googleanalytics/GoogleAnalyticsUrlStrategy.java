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

import org.jboss.tools.usage.util.EncodingUtils;

/**
 * Class that builds an URL that passes given parameters to google analytics
 * 
 * @author Andre Dietisheim
 */
public class GoogleAnalyticsUrlStrategy implements IURLBuildingStrategy {

	private static final String TRACKING_URL = "http://www.google-analytics.com/__utm.gif";

	private static final int VISITS = -1;

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

		/*
		 * our working tracking code
		 * 
		http://www.google-analytics.com/__utm.gif?utmwv=4.7.2
		&utmn=338321265
		&utmhn=jboss.org
		&utmcs=UTF-8
		&utmsr=1920x1080
		&utmsc=24-bit
		&utmul=en-us
		&utmje=1
		&utmfl=10.1%20r53
		&utmdt=-%20JBoss%20Community
		&utmhid=1087431432
		&utmr=0
		&utmp=%2Ftools%2Fusage.html
		&utmac=UA-17645367-1
		&utmcc=__utma%3D156030500.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B
		&gaq=1
		 *
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
		appendParameter(IGoogleAnalyticsParameters.PARAM_HID, getRandomNumber(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_REFERRAL, googleParameters.getReferral(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_PAGE_REQUEST, focusPoint.getContentURI(), builder);

		appendParameter(IGoogleAnalyticsParameters.PARAM_ACCOUNT_NAME, googleParameters.getAccountName(), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_COOKIES, getCookies(focusPoint), builder);
		appendParameter(IGoogleAnalyticsParameters.PARAM_GAQ, "1", false, builder);

		return builder.toString();
	}

	private String getCookies(FocusPoint focusPoint) {
 		long timeStamp = System.currentTimeMillis();
		StringBuilder builder = new StringBuilder();
		builder.append(IGoogleAnalyticsParameters.PARAM_COOKIES_FIRST_VISIT)
				.append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append("999.")
				.append(googleParameters.getUserId()).append(IGoogleAnalyticsParameters.DOT)
				.append(timeStamp).append(IGoogleAnalyticsParameters.DOT)
				.append(timeStamp).append(IGoogleAnalyticsParameters.DOT)
				.append(timeStamp).append(IGoogleAnalyticsParameters.DOT)
				.append(VISITS)
				.append(IGoogleAnalyticsParameters.SEMICOLON)
				.append(IGoogleAnalyticsParameters.PLUS_SIGN)
				.append(IGoogleAnalyticsParameters.PARAM_COOKIES_REFERRAL_TYPE)
				.append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append("999.")
				.append(timeStamp).append(IGoogleAnalyticsParameters.DOT)
				.append("1.1.")
				.append(IGoogleAnalyticsParameters.PARAM_COOKIES_UTMCSR).append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append("(direct)").append(IGoogleAnalyticsParameters.PIPE)
				.append(IGoogleAnalyticsParameters.PARAM_COOKIES_UTMCCN).append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append("(direct)").append(IGoogleAnalyticsParameters.PIPE)
				.append(IGoogleAnalyticsParameters.PARAM_COOKIES_UTMCMD).append(IGoogleAnalyticsParameters.EQUALS_SIGN)
				.append("(none)");
		appendCookieKeyword(builder);
		builder.append(IGoogleAnalyticsParameters.SEMICOLON);

		return EncodingUtils.checkedEncodeUtf8(builder.toString());

		// builder.append(IGoogleAnalyticsParameters.PARAM_COOKIE_VALUES)
		// .append(IGoogleAnalyticsParameters.EQUALS_SIGN)
		// .append("__utma%3D")
		// .append(getRandomNumber())
		// .append(".")
		// .append(getRandomNumber())
		// .append(".")
		// .append(now)
		// .append(".")
		// .append(now)
		// .append(".")
		// .append(now)
		// .append(".2%3B%2B)")
		//
		// // .append("__utmb%3D")
		// // .append(getRandomNumber())
		// // .append("%3B%2B__utmc%3D")
		// // .append(getRandomNumber())
		// // .append("%3B%2B")
		//
		// .append("__utmz%3D")
		// .append(getRandomNumber())
		// .append(".")
		// .append(now)
		// .append(IGoogleAnalyticsParameters.AMPERSAND);

		// .append("utmcsr%3D(direct)%7C")
		// .append("utmccn%3D(direct)%7C")
		// .append("utmcmd%3D(none)%3B");

	}

	/**
	 * Appends the keyword to the cookies.
	 *
	 * @param builder the builder to append to
	 */
	private void appendCookieKeyword(StringBuilder builder) {
		String keyword = googleParameters.getKeyword();
		if (keyword != null && keyword.length() > 0) {
			builder.append(IGoogleAnalyticsParameters.PIPE)
					.append(IGoogleAnalyticsParameters.PARAM_COOKIES_KEYWORD)
					.append(IGoogleAnalyticsParameters.EQUALS_SIGN)
					.append(keyword);
		}
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

	// private String getIpAddress() throws SocketException {
	// Enumeration<NetworkInterface> e1 =
	// (Enumeration<NetworkInterface>)NetworkInterface.getNetworkInterfaces();
	// while(e1.hasMoreElements()) {
	// NetworkInterface ni = e1.nextElement();
	//			
	// System.out.print(ni.getName());
	// System.out.print(" : [");
	// Enumeration<InetAddress> e2 = ni.getInetAddresses();
	// while(e2.hasMoreElements()) {
	// InetAddress ia = e2.nextElement();
	// System.out.print(ia);
	// if( e2.hasMoreElements()) {
	// System.out.print(",");
	// }
	// }
	// System.out.println("]");
	// }
	// }
}
