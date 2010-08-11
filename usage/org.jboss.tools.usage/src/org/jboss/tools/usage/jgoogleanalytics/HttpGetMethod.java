package org.jboss.tools.usage.jgoogleanalytics;

import java.io.IOException;
import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.MessageFormat;

/**
 * Simple class peforming HTTP Get method on the requested url
 * 
 * @author Siddique Hameed
 * @author Andre Dietisheim
 * @version 0.2
 */

public class HttpGetMethod {
	private static final String USER_AGENT = "User-Agent";
	private static final String GET_METHOD_NAME = "GET";
	private static final String SUCCESS_MESSAGE = "Http Get to url {0} successfull!";
	private static final String ERROR_MESSAGE = "Http Get to {0} failed, response code was {1}";

	private ILoggingAdapter loggingAdapter = null;

	private CookieManager cookieHandler;
	private String userAgent;

	public HttpGetMethod(String userAgent) {
		this.userAgent = userAgent;
		this.cookieHandler = new CookieManager();
		cookieHandler.setCookiePolicy(CookiePolicy.ACCEPT_ALL);
	}

	public void setLoggingAdapter(ILoggingAdapter loggingAdapter) {
		this.loggingAdapter = loggingAdapter;
	}

	public void request(String urlString) {

		CookieHandler currentCookieHandler = setCookieHandler(cookieHandler);
		try {
			HttpURLConnection urlConnection = createURLConnection(urlString, userAgent);
			urlConnection.connect();
			int responseCode = getResponseCode(urlConnection);
			if (responseCode != HttpURLConnection.HTTP_OK) {
				logError(ERROR_MESSAGE, urlString, responseCode);
			} else {
				logMessage(SUCCESS_MESSAGE, urlString);
			}
		} catch (Exception e) {
			logError(e.getMessage());
		} finally {
			setCookieHandler(currentCookieHandler);
		}
	}

	/**
	 * Returns the return code from the given {@link HttpURLConnection}.
	 * Provided to be called by test cases so that they can retrieve the return code.
	 * 
	 * @param urlConnection
	 *            to get the response code from
	 * @return the return code the HttpUrlConnection received
	 * @throws IOException
	 */
	protected int getResponseCode(HttpURLConnection urlConnection) throws IOException {
		return urlConnection.getResponseCode();
	}

	private CookieHandler setCookieHandler(CookieHandler cookieHandler) {
		CookieHandler currentCookieHandler = CookieHandler.getDefault();
		CookieHandler.setDefault(cookieHandler);
		return currentCookieHandler;
	}

	protected HttpURLConnection createURLConnection(String urlString, String userAgent) throws IOException {
		URL url = new URL(urlString);
		HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
		urlConnection.setInstanceFollowRedirects(true);
		urlConnection.setRequestMethod(GET_METHOD_NAME);
		urlConnection.setRequestProperty(USER_AGENT, userAgent);
		return urlConnection;
	}

	private void logMessage(String message, Object... parameters) {
		if (loggingAdapter != null) {
			loggingAdapter.logMessage(MessageFormat.format(message, parameters));
		}
	}

	private void logError(String errorMesssage, Object... parameters) {
		if (loggingAdapter != null) {
			loggingAdapter.logError(MessageFormat.format(errorMesssage, parameters));
		}
	}
}
