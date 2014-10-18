/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.util;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

/**
 * HTTP Utilities
 * @author Alexey Kazakov
 */
public class HttpUtil {

	private static IProxyService proxyService;

	/**
	 * @param url
	 * @return InputStream of the response body of the http GET request. Proxy settings from preferences is used.
	 * @throws Exception
	 * @deprecated Use getInputStreamReader(HttpURLConnection connection)
	 */
	@Deprecated
	public static InputStream getInputStreamFromUrlByGetMethod(String url) throws IOException {
		return getInputStreamFromUrlByGetMethod(url, false);
	}

	/**
	 * Returns the response body of the Get HTTP method. If checkStatusCode==true then return null if the status code is not OK (200)
	 * Uses proxy settings from preferences.
	 * @param url
	 * @return 
	 * @throws IOException
	 * @deprecated Use getInputStreamReader(HttpURLConnection connection)
	 */
	@Deprecated
	public static InputStream getInputStreamFromUrlByGetMethod(String url, boolean checkStatusCode) throws IOException {
		InputStream is = null;
		GetMethod method = executeGetMethod(url);
		if(!checkStatusCode || method.getStatusCode()==HttpStatus.SC_OK) {
			is = method.getResponseBodyAsStream();
		}
		return is;
	}

	/**
	 * Create and open an HTTP connection (GET). If the response code is 200 (OK) then returns an input stream reader that reads from this open connection.
	 * Otherwise returns null.
	 * @param httpUrl
	 * @param timeout
	 * @return
	 * @throws IOException
	 */
	public static InputStreamReader getInputStreamReader(String httpUrl, int timeout) throws IOException {
		HttpURLConnection connection = createHttpURLConnection(httpUrl, timeout);
		if(connection!=null) {
			return getInputStreamReader(connection);
		}
		return null;
	}

	/**
	 * Open the HTTP connection. If the response code is 200 (OK) then returns an input stream reader that reads from this connection.
	 * Otherwise returns null.
	 * @param httpUrl
	 * @param timeout
	 * @return
	 * @throws IOException
	 */
	public static InputStreamReader getInputStreamReader(HttpURLConnection connection) throws IOException {
		return getInputStreamReader(connection, 0);
	}

	/**
	 * Create an HTTP connection (GET). Returns null if the URL does not use HTTP(S) protocol.
	 * @param urlString
	 * @param timeout
	 * @return
	 * @throws IOException
	 */
	public static HttpURLConnection createHttpURLConnection(String urlString, int timeout) throws IOException {
		URL url = new URL(urlString);
		URLConnection connetion = url.openConnection();
		HttpURLConnection httpConnection = null;
		if(connetion instanceof HttpURLConnection) {
			httpConnection = (HttpURLConnection) url.openConnection();
			httpConnection.setInstanceFollowRedirects(true);
			httpConnection.setRequestMethod("GET");
			httpConnection.setConnectTimeout(timeout);
		}
		return httpConnection;
	}

	private static InputStreamReader getInputStreamReader(HttpURLConnection connection, int redirectAttemptCount) throws IOException {
		InputStreamReader responseReader = null;
		connection.connect();
		int responseCode = connection.getResponseCode();
		if (responseCode == HttpURLConnection.HTTP_OK) { // OK
			String contentTypeCharset = null;
			String contentType = connection.getContentType();
			if(contentType!=null) {
				Matcher matcher = Pattern.compile("charset=(.+)").matcher(contentType);
				if (matcher.find()) {
					contentTypeCharset = matcher.group(1);
				}
			}
			InputStream inputStream = connection.getInputStream();
			if (contentTypeCharset != null && contentTypeCharset.length() > 0) {
				responseReader = new InputStreamReader(new BufferedInputStream(inputStream), contentTypeCharset);
			} else {
				responseReader = new InputStreamReader(new BufferedInputStream(inputStream));
			}
		} else if(responseCode >= 300 && responseCode < 400) { // Redirect
			// URLConnection does not redirect automatically if the protocol is different. HTTP -> HTTPS for example.
			// So we have to do it manually.
			String redirectLocation = connection.getHeaderField("location");
			if(redirectLocation!=null && !connection.getURL().toString().equalsIgnoreCase(redirectLocation.trim())) { // Ignore responses with empty redirect locations or with the same redirect URL
				redirectAttemptCount++;
				if(redirectAttemptCount>1) {
					// Only one redirect is allowed.
					HttpURLConnection redirectConnection = createHttpURLConnection(redirectLocation, connection.getConnectTimeout());
					redirectConnection.setIfModifiedSince(connection.getIfModifiedSince());
					return getInputStreamReader(redirectConnection, redirectAttemptCount);
				}
			}
		}
		return responseReader;
	}

	/**
	 * @param url
	 * @return InputStream of response to http GET request. Use proxy settings from preferences.
	 * @throws Exception
	 */
	public static InputStream getInputStreamFromUrlByGetMethod(String url, IProxyService proxyService) throws IOException {
		InputStream is = executeGetMethod(url, proxyService).getResponseBodyAsStream();
		return is;
	}

	/**
	 * @param url
	 * @param proxyService
	 * @return Status code of response to http GET request. Use given proxy settings.
	 * @throws Exception
	 */
	public static int getStatusCodeFromUrlByGetMethod(String url, IProxyService proxyService) throws IOException {
		int code = executeGetMethod(url, proxyService).getStatusCode();
		return code;
	}

	/**
	 * @param url
	 * @return Status code of response to http GET request. Use proxy settings from preferences.
	 * @throws Exception
	 */
	public static int getStatusCodeFromUrlByGetMethod(String url) throws IOException {
		int code = executeGetMethod(url).getStatusCode();
		return code;
	}

	/**
	 * @param url
	 * @return InputStream of response to http POST request. Use proxy settings from preferences.
	 * @throws Exception
	 */
	public static InputStream getInputStreamFromUrlByPostMethod(String url) throws IOException {
		InputStream is = executePostMethod(url).getResponseBodyAsStream();
		return is;
	}

	/**
	 * @param url
	 * @param proxyService
	 * @return Status code of response to http POST request. Use given proxy settings.
	 * @throws Exception
	 */
	public static int getStatusCodeFromUrlByPostMethod(String url, IProxyService proxyService) throws IOException {
		int code = executePostMethod(url, proxyService).getStatusCode();
		return code;
	}

	/**
	 * @param url
	 * @return Status code of response to http POST request. Use proxy settings from preferences.
	 * @throws Exception
	 */
	public static int getStatusCodeFromUrlByPostMethod(String url) throws IOException {
		int code = executePostMethod(url).getStatusCode();
		return code;
	}

	private static GetMethod executeGetMethod(String url) throws IOException {
		IProxyService proxyService = getProxyService();
		return executeGetMethod(url, proxyService);
	}

	private static PostMethod executePostMethod(String url) throws IOException {
		IProxyService proxyService = getProxyService();
		return executePostMethod(url, proxyService);
	}

	/**
	 * @param url
	 * @param proxyService
	 * @return
	 * @throws Exception
	 */
	public static GetMethod executeGetMethod(String url, IProxyService proxyService) throws IOException {
		GetMethod httpGet = new GetMethod(url);
		HttpClient httpClient = createHttpClient(url, proxyService);
		httpClient.executeMethod(httpGet);
		return httpGet;
	}

	private static PostMethod executePostMethod(String url, IProxyService proxyService) throws IOException {
		PostMethod httpPost = new PostMethod(url);
		HttpClient httpClient = createHttpClient(url, proxyService);
	    httpClient.executeMethod(httpPost);
		return httpPost;
	}

	/**
	 * @param url
	 * @return HttpClient with internet proxy settings;
	 * @throws Exception
	 */
	public static HttpClient createHttpClient(String url) throws IOException {
		return createHttpClient(url, getProxyService());
	}

	public static HttpClient createHttpClient(String url, int connectionTimeout) throws IOException {
		return createHttpClient(url, getProxyService(), connectionTimeout);
	}

	private static HttpClient createHttpClient(String url, IProxyService proxyService) throws IOException {
	    return createHttpClient(url, proxyService, 30000);
	}

	private static HttpClient createHttpClient(String url, IProxyService proxyService, int connectionTimeout) throws IOException {
		HttpClient httpClient = new HttpClient();

		if(proxyService.isProxiesEnabled()) {
			IProxyData[] proxyData = proxyService.getProxyData();
			URL netUrl = new URL(url);
			String hostName = netUrl.getHost();
			String[] nonProxiedHosts = proxyService.getNonProxiedHosts();
			boolean nonProxiedHost = false;
			for (int i = 0; i < nonProxiedHosts.length; i++) {
				String nonProxiedHostName = nonProxiedHosts[i];
				if(nonProxiedHostName.equalsIgnoreCase(hostName)) {
					nonProxiedHost = true;
					break;
				}
			}
			if(!nonProxiedHost) {
				for (int i = 0; i < proxyData.length; i++) {
					IProxyData proxy = proxyData[i];
					if(IProxyData.HTTP_PROXY_TYPE.equals(proxy.getType())) {
						String proxyHostName = proxy.getHost();
						if(proxyHostName==null) {
							break;
						}
						int portNumber = proxy.getPort();
						if(portNumber==-1) {
							portNumber = 80;
						}
						httpClient.getHostConfiguration().setProxy(proxyHostName, portNumber);
						if(proxy.isRequiresAuthentication()) {
						    String userName = proxy.getUserId();
						    if(userName!=null) {
							    String password = proxy.getPassword();
							    httpClient.getState().setProxyCredentials(new AuthScope(null, AuthScope.ANY_PORT, null, AuthScope.ANY_SCHEME), new UsernamePasswordCredentials(userName, password));
						    }
						}
						break; // Use HTTP proxy only.
					}
				}
			}
		}

	    httpClient.getHttpConnectionManager().getParams().setConnectionTimeout(connectionTimeout);

	    return httpClient;
	}

	private static IProxyService getProxyService() {
		if(proxyService==null) {
			BundleContext bundleContext = CommonCorePlugin.getInstance().getBundle().getBundleContext();
			ServiceReference serviceReference = bundleContext.getServiceReference(IProxyService.class.getName());
			proxyService = (IProxyService)bundleContext.getService(serviceReference);
		}
		return proxyService;
	}
}