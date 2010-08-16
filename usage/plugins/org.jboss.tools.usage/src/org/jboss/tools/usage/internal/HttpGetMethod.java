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
package org.jboss.tools.usage.internal;

import java.io.IOException;
import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.MessageFormat;

import org.jboss.tools.usage.googleanalytics.ILoggingAdapter;

/**
 * Class that executes a HTTP Get request to the given url.
 * 
 * @author Andre Dietisheim
 */
public class HttpGetMethod {
	
	private static final String USER_AGENT = "User-Agent";

	private static final String GET_METHOD_NAME = "GET";
	
	private static final String SUCCESS_MESSAGE = "Http Get to url {0} successfull!";
	
	private static final String ERROR_MESSAGE = "Http Get to {0} failed, response code was {1}";

	private ILoggingAdapter loggingAdapter = null;

	private CookieManager cookieHandler;
	
	private String userAgent;

	public HttpGetMethod(String userAgent, ILoggingAdapter loggingAdapter) {
		this.userAgent = userAgent;
		this.loggingAdapter = loggingAdapter;
		this.cookieHandler = new CookieManager();
		cookieHandler.setCookiePolicy(CookiePolicy.ACCEPT_ALL);
	}

	public void request(String urlString) {

		CookieHandler currentCookieHandler = setCookieHandler(cookieHandler);
		try {
			HttpURLConnection urlConnection = createURLConnection(urlString, userAgent);
			urlConnection.connect();
			int responseCode = getResponseCode(urlConnection);
			if (responseCode != HttpURLConnection.HTTP_OK) {
				loggingAdapter.logMessage(MessageFormat.format(ERROR_MESSAGE, urlString, responseCode));
			} else {
				loggingAdapter.logError(MessageFormat.format(SUCCESS_MESSAGE, urlString));
			}
		} catch (Exception e) {
			loggingAdapter.logError(e.getMessage());
		} finally {
			setCookieHandler(currentCookieHandler);
		}
	}

	/**
	 * Returns the return code from the given {@link HttpURLConnection}.
	 * Provided to be called by test cases so that they can retrieve the return code.
	 *
	 * @param urlConnection to get the response code from
	 * @return the return code the HttpUrlConnection received
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	protected int getResponseCode(HttpURLConnection urlConnection) throws IOException {
		return urlConnection.getResponseCode();
	}

	private CookieHandler setCookieHandler(CookieHandler cookieHandler) {
		CookieHandler currentCookieHandler = CookieHandler.getDefault();
		CookieHandler.setDefault(cookieHandler);
		return currentCookieHandler;
	}

	/**
	 * Creates a new url connection.
	 *
	 * @param urlString the url string
	 * @param userAgent the user agent
	 * @return the http url connection
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	protected HttpURLConnection createURLConnection(String urlString, String userAgent) throws IOException {
		URL url = new URL(urlString);
		HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
		urlConnection.setInstanceFollowRedirects(true);
		urlConnection.setRequestMethod(GET_METHOD_NAME);
		urlConnection.setRequestProperty(USER_AGENT, userAgent);
		return urlConnection;
	}
}
