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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.util.HttpEncodingUtils;
import org.jboss.tools.usage.util.StatusUtils;

/**
 * A class that implements a global reporting enablement setting. The current
 * implementation queries a given url and extracts the enablement value out of
 * the response.
 */
public class GlobalUsageReportingSettings {

	private static final String REPORTING_ENABLEMENT_ENABLEDVALUE = "ENABLED";

	private static final String GET_METHOD_NAME = "GET"; //$NON-NLS-1$
	private static final String REPORTING_ENABLEMENT_URL = "http://community.jboss.org/wiki/JBossToolsJBossDeveloperStudioUsageReportingEnablement"; //$NON-NLS-1$
	private static final String REPORTING_ENABLEMENT_STARTSEQUENCE = "Usage Reporting is "; //$NON-NLS-1$

	private Plugin plugin;
	private HttpURLConnection urlConnection;

	public GlobalUsageReportingSettings(Plugin plugin) throws IOException {
		Assert.isNotNull(plugin);

		this.plugin = plugin;
		this.urlConnection = createURLConnection(REPORTING_ENABLEMENT_URL);
	}

	public boolean isEnabled() {
		try {
			return parse(request(REPORTING_ENABLEMENT_URL));
		} catch (IOException e) {
			IStatus status = StatusUtils.getErrorStatus(
					plugin.getBundle().getSymbolicName()
					, UsageMessages.KillSwitchPreference_Error_Exception, e);
			plugin.getLog().log(status);
			return false;
		}
	}

	/**
	 * Sends a http GET request to the given URL. Returns the response string or
	 * <tt>null</tt> if an error occurred. The errors catched are Exceptions or
	 * HTTP error codes.
	 * 
	 * @param url
	 *            the url to send the GET request to
	 * @return the response or <tt>null</tt> if an error occured.
	 * @throws UnsupportedEncodingException
	 * 
	 * @see HttpURLConnection
	 */
	protected InputStreamReader request(String url) throws UnsupportedEncodingException {
		InputStreamReader responseReader = null;
		try {
			urlConnection.connect();
			int responseCode = getResponseCode(urlConnection);
			if (responseCode == HttpURLConnection.HTTP_OK) {
				IStatus status = StatusUtils.getDebugStatus(
						plugin.getBundle().getSymbolicName()
						, UsageMessages.KillSwitchPreference_Info_HttpQuery
						, url);
				plugin.getLog().log(status);
				responseReader = getInputStreamReader(urlConnection.getContentType());
			} else {
				IStatus status = StatusUtils.getErrorStatus(
						plugin.getBundle().getSymbolicName()
						, UsageMessages.KillSwitchPreference_Error_Http, null, url);
				plugin.getLog().log(status);
			}
		} catch (Exception e) {
			IStatus status = StatusUtils.getErrorStatus(
					plugin.getBundle().getSymbolicName()
					, UsageMessages.KillSwitchPreference_Error_Http, e, url);
			plugin.getLog().log(status);
		}
		return responseReader;
	}

	private InputStreamReader getInputStreamReader(String contentType) throws UnsupportedEncodingException, IOException {
		String contentTypeCharset = HttpEncodingUtils.getContentTypeCharset(contentType);
		if (contentTypeCharset != null && contentTypeCharset.length() > 0) {
			return new InputStreamReader(new BufferedInputStream(urlConnection.getInputStream()),
					contentTypeCharset);
		} else {
			return new InputStreamReader(new BufferedInputStream(urlConnection.getInputStream()));
		}
	}

	/**
	 * Parses the given string and extracts the enablement value.
	 * 
	 * @param input
	 *            stream that holds
	 * @return true, if successful
	 */
	private boolean parse(InputStreamReader reader) throws IOException {
		int i = 0;
		char[] reportingValueCharacters = new char[REPORTING_ENABLEMENT_ENABLEDVALUE.length()];
		for (int character = 0; character != -1; character = reader.read()) {
			if (REPORTING_ENABLEMENT_STARTSEQUENCE.charAt(i) == (char) character) {
				if (++i == REPORTING_ENABLEMENT_STARTSEQUENCE.length()) {
					reader.read(reportingValueCharacters);
					return isEnabled(new String(reportingValueCharacters));
				}
			} else {
				i = 0;
			}
		}
		return false;
	}

	private boolean isEnabled(String enablementValue) {
		return (enablementValue != null && REPORTING_ENABLEMENT_ENABLEDVALUE.equals(enablementValue.toUpperCase()));
	}

	/**
	 * Creates a new url connection.
	 * 
	 * @param urlString
	 *            the url string
	 * @return the http url connection
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	protected HttpURLConnection createURLConnection(String urlString) throws IOException {
		URL url = new URL(urlString);
		HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
		urlConnection.setInstanceFollowRedirects(true);
		urlConnection.setRequestMethod(GET_METHOD_NAME);
		return urlConnection;
	}

	/**
	 * Returns the return code from the given {@link HttpURLConnection}.
	 * Provided to be called by test cases so that they can retrieve the return
	 * code.
	 * 
	 * @param urlConnection
	 *            to get the response code from
	 * @return the return code the HttpUrlConnection received
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	protected int getResponseCode(HttpURLConnection urlConnection) throws IOException {
		return urlConnection.getResponseCode();
	}
}
