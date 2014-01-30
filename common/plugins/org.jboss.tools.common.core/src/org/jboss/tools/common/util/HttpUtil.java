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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

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
	 */
	public static InputStream getInputStreamFromUrlByGetMethod(String url) throws IOException {
		return getInputStreamFromUrlByGetMethod(url, false);
	}

	/**
	 * Returns the response body of the Get HTTP method. If checkStatusCode==true then return null if the status code is not OK (200)
	 * Uses proxy settings from preferences.
	 * @param url
	 * @return 
	 * @throws IOException
	 */
	public static InputStream getInputStreamFromUrlByGetMethod(String url, boolean checkStatusCode) throws IOException {
		InputStream is = null;
		GetMethod method = executeGetMethod(url);
		if(!checkStatusCode || method.getStatusCode()==HttpStatus.SC_OK) {
			is = method.getResponseBodyAsStream();
		}
		return is;
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