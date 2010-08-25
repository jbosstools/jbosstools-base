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
package org.jboss.tools.usage;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.MessageFormat;


/**
 * Class that executes a HTTP Get request to the given url.
 * 
 * @author Andre Dietisheim
 */
public class HttpGetRequest implements IHttpGetRequest {
	
	private static final String USER_AGENT = "User-Agent"; //$NON-NLS-1$

	private static final String GET_METHOD_NAME = "GET"; //$NON-NLS-1$
	
	private ILoggingAdapter loggingAdapter = null;

//	private CookieHandler cookieHandler;
	
	private String userAgent;

	public HttpGetRequest(String userAgent, ILoggingAdapter loggingAdapter) {
		this.userAgent = userAgent;
		this.loggingAdapter = loggingAdapter;
//		this.cookieHandler = new CookieHandler();
//		this.cookieHandler = CookieHandler.getDefault();
//		cookieHandler.setCookiePolicy(CookiePolicy.ACCEPT_ALL);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.usage.IHttpGetRequest#request(java.lang.String)
	 */
	public void request(String urlString) {

//		CookieHandler currentCookieHandler = setCookieHandler(cookieHandler);
		try {
			HttpURLConnection urlConnection = createURLConnection(urlString, userAgent);
			urlConnection.connect();
			int responseCode = getResponseCode(urlConnection);
			if (responseCode == HttpURLConnection.HTTP_OK) {
				loggingAdapter.logMessage(MessageFormat.format(UsageMessages.HttpGetMethod_Success, urlString, responseCode));
			} else {
				loggingAdapter.logError(MessageFormat.format(UsageMessages.HttpGetMethod_Error_Http, urlString));
			}
		} catch (Exception e) {
			loggingAdapter.logMessage(MessageFormat.format(UsageMessages.HttpGetMethod_Error_Io, urlString, e.toString()));
		} finally {
//			setCookieHandler(currentCookieHandler);
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

//	private CookieHandler setCookieHandler(CookieHandler cookieHandler) {
//		CookieHandler currentCookieHandler = CookieHandler.getDefault();
//		CookieHandler.setDefault(cookieHandler);
//		return currentCookieHandler;
//	}

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
