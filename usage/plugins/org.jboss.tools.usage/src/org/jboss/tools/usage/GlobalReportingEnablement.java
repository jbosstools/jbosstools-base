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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.util.StatusUtils;

/**
 * A class that implements a global reporting enablement setting. The current
 * implementation queries a given url and extracts the enablement value out of
 * the response.
 */
public class GlobalReportingEnablement {

	private static final String GET_METHOD_NAME = "GET"; //$NON-NLS-1$
	private static final String REPORTING_ENABLEMENT_URL = "https://community.jboss.org/wiki/JBossToolsJBossDeveloperStudioUsageReportingEnablement"; //$NON-NLS-1$
	private static final String REPORTING_ENABLEMENT_REGEX = "Usage Reporting is ([A-Za-z]+)"; //$NON-NLS-1$

	private Plugin plugin;
	private HttpURLConnection urlConnection;
	private Pattern pattern;

	public GlobalReportingEnablement(Plugin plugin) throws IOException {
		Assert.isNotNull(plugin);

		this.plugin = plugin;
		this.urlConnection = createURLConnection(REPORTING_ENABLEMENT_URL);
		this.pattern = Pattern.compile(REPORTING_ENABLEMENT_REGEX);
	}

	public boolean isEnabled() {
		return parse(request(REPORTING_ENABLEMENT_URL));
	}

	/**
	 * Sends a http GET request to the given URL. Returns the response string or
	 * <tt>null</tt> if an error occurred. The errors catched are Exceptions or
	 * HTTP error codes.
	 * 
	 * @param url
	 *            the url to send the GET request to
	 * @return the response or <tt>null</tt> if an error occured.
	 * 
	 * @see HttpURLConnection
	 */
	protected String request(String url) {
		String response = null;
		try {
			urlConnection.connect();
			int responseCode = getResponseCode(urlConnection);
			if (responseCode == HttpURLConnection.HTTP_OK) {
				IStatus status = StatusUtils.getDebugStatus(
						plugin.getBundle().getSymbolicName()
						, UsageMessages.KillSwitchPreference_Info_HttpQuery, url);
				plugin.getLog().log(status);
				response = urlConnection.getResponseMessage();
			} else {
				IStatus status = StatusUtils.getErrorStatus(
						plugin.getBundle().getSymbolicName()
						, UsageMessages.KillSwitchPreference_Error_Http, null, url);
				plugin.getLog().log(status);
			}
		} catch (Exception e) {
			IStatus status = StatusUtils.getErrorStatus(
					plugin.getBundle().getSymbolicName()
					, UsageMessages.KillSwitchPreference_Error_Http, null, url);
			plugin.getLog().log(status);
		}
		return response;
	}

	/**
	 * Parses the given string and extracts the enablement value.
	 * 
	 * @param response
	 *            the response
	 * @return true, if successful
	 */
	private boolean parse(String response) {
		Matcher matcher = pattern.matcher(response);
		if (matcher.find() && matcher.groupCount() >= 1) {
			String enablementValue = matcher.group(1);
			return isEnabled(enablementValue);
		}
		return false;
	}

	private boolean isEnabled(String enablementValue) {
		return (enablementValue != null && "ENABLED".equals(enablementValue.toUpperCase()));
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
