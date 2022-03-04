/*************************************************************************************
 * Copyright (c) 2013-2019 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.foundation.core.test.ecf;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.core.internal.FoundationCorePlugin;
import org.jboss.tools.foundation.core.test.FoundationTestConstants;
import org.junit.Ignore;
import org.junit.Test;
import org.osgi.framework.Bundle;

import junit.framework.TestCase;

public class URLTransportUtilTest extends TestCase {
	private static final int STATUS_200_OK = 200;
	private static final int DEFAULT_DELAY = 45000;

	private void testDownload(String urlString) {
		testDownload(urlString, new NullProgressMonitor());
	}
	
	private void testDownload(String urlString, IProgressMonitor monitor) {
		URLTransportUtility util = new URLTransportUtility();
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		IStatus s = util.download("displayName", urlString, baos, monitor);
		
		assertTrue(s.isOK());
		String stringFromDownload = baos.toString();
		assertNotNull(stringFromDownload);
		assertNotNull(stringFromDownload);
	}
	
	private void testLastModified(String urlString) {
		// verify get last timestamp works, or at least doesn't fail with an exception
		URLTransportUtility util = new URLTransportUtility();
		try {
			long l = util.getLastModified(new URL(urlString));
		} catch(CoreException ce) {
			fail(ce.getMessage());
		} catch(MalformedURLException murle) {
			fail(murle.getMessage());
		}
	}
	
	@Test
	public void testWebDownload() {
		String urlString = "https://raw.github.com/jboss-jdf/jdf-stack/1.0.0.Final/stacks.yaml";
		testDownload(urlString);
		testLastModified(urlString);
	}
	
	@Test
	public void testLongUrl() throws Exception {
		executeWithHttpServer(server -> {
			String urlString = server.getURI()
					+ "wearejustdoingthistobestupidnowsincethiscangoonforeverandeverandeverbutitstilllookskindaneatinthebrowsereventhoughitsabigwasteoftimeandenergyandhasnorealpointbutwehadtodoitanyways.html";
			File file = new URLTransportUtility().getCachedFileForURL(urlString, "Long Url",
					URLTransportUtility.CACHE_UNTIL_EXIT, new NullProgressMonitor());
			assertTrue(file.exists());
			assertTrue(file.length() > 0);
		}, STATUS_200_OK, 0);
	}
	
	@Test
	public void testUserAgent() throws Exception {
		UserAgentHandler handler = new UserAgentHandler();
		Server server = startFakeHttpServer(handler, STATUS_200_OK, 1);
		String urlString = server.getURI()
				+ "checkUserAgent.html";
		File file = new URLTransportUtility().getCachedFileForURL(urlString, "checkUserAgent",
				URLTransportUtility.CACHE_UNTIL_EXIT, new NullProgressMonitor());
		assertTrue(file.exists());
		assertTrue(file.length() > 0);
		String userAgent = handler.getUserAgent();
		boolean matches = userAgent.startsWith("jbosstools/") || userAgent.startsWith("devstudio/");
		assertTrue(matches);
	}

	
	private static class UserAgentHandler extends AbstractHandler {
		
		private String userAgent;

		@Override
		public void handle(String target, Request baseRequest, HttpServletRequest request,
				HttpServletResponse response) throws IOException, ServletException {
			System.out.println("Responding to request: hello user agent");
			response.getWriter().print("hello user agent");
			this.userAgent = (request.getHeader("User-Agent"));
			baseRequest.setHandled(true);
		}

		public String getUserAgent() {
			return userAgent;
		}
	}
	
	protected AbstractHandler getDefaultHandler(long delay) {
		return new AbstractHandler() {

			@Override
			public void handle(String target, Request baseRequest, HttpServletRequest request,
					HttpServletResponse response) throws IOException, ServletException {
				System.out.println("Response requested. Delaying");
				try {
					Thread.sleep(delay);
				} catch(InterruptedException ie) {}
				System.out.println("Responding to request: Hello");
				response.getWriter().print("Hello");
				baseRequest.setHandled(true);
			}
		};
	}
	protected Server startFakeHttpServer(int status, final long delay) throws Exception {
		return startFakeHttpServer(getDefaultHandler(delay), status, delay);
	}
	protected Server startFakeHttpServer(AbstractHandler handler, int status, final long delay) throws Exception {
		Server server = new Server(0);
		server.setHandler(handler);
		server.start();
		return server;
	}
	
	@Test
	public void testTimeoutWithoutCancel() throws Exception {
		executeWithHttpServer(server -> {
			try {
				String serverUrl = server.getURI().toURL().toExternalForm();
				String url = URI.create(serverUrl.substring(0, serverUrl.lastIndexOf(':'))).toURL().toExternalForm()+"/";
				final IProgressMonitor monitor = new NullProgressMonitor();
				URLTransportUtility util = new URLTransportUtility();
				ByteArrayOutputStream os = new ByteArrayOutputStream();
				long t1 = System.currentTimeMillis();
				IStatus s = util.download("displayString", url, os, 500, monitor);
				long t2 = System.currentTimeMillis();
				assertTrue(s.getSeverity() == IStatus.ERROR);
				assertTrue(t2 - t1 < 3000);
			} catch(MalformedURLException murle) {
			}
		}, STATUS_200_OK, DEFAULT_DELAY);

	}
	
	@Test
	public void testCancelMonitor() throws Exception {
		executeWithHttpServer(server -> {
			final Long[] times = new Long[2]; 	
			
			try {
				URL url = server.getURI().toURL();
				final IProgressMonitor monitor = new NullProgressMonitor();
				new Thread() {	
					public void run() {
						System.out.println("Cancel Monitor Thread Launched");
						try {
							Thread.sleep(3000);
						} catch(InterruptedException ie) {}
						System.out.println("Canceling Monitor");
						synchronized (times) {
							times[0] = System.currentTimeMillis();
						}
						monitor.setCanceled(true);
					}
				}.start();
				URLTransportUtility util = new URLTransportUtility();
				ByteArrayOutputStream os = new ByteArrayOutputStream();
				IStatus s = util.download("displayString", url.toExternalForm(), os, monitor);
				synchronized (times) {
					times[1] = System.currentTimeMillis();				
				}
				assertTrue(s.getSeverity() == IStatus.CANCEL || s.getSeverity() == IStatus.ERROR);
				// MUST cancel within 500 ms
				assertTrue(times[1] - times[0] < 500);
			} catch(MalformedURLException murle) {
				fail(murle.getMessage());
			} 
		}, STATUS_200_OK, DEFAULT_DELAY);
	}

	@Test
	public void testTimeToLive1() throws Exception {
		testTimeToLive(1000);
	}
	
	@Test
	public void testTimeToLive2() throws Exception {
		testTimeToLive(5000);
	}

	
	private void testTimeToLive(long timeout) throws Exception {
		executeWithHttpServer(server -> {
			
			try {
				URL url = server.getURI().toURL();
				long start = System.currentTimeMillis();
				URLTransportUtility util = new URLTransportUtility();
				File result = util.getCachedFileForURL( url.toExternalForm(), "displayName", URLTransportUtility.CACHE_UNTIL_EXIT, 20000, timeout, new NullProgressMonitor());
				long finish = System.currentTimeMillis();
				long duration = finish - start;
				assertTrue(duration > timeout); // Should be impossible to finish BEFORE the timeout
				assertTrue(duration < (timeout + 300));  // Add 300 ms in case of jobs taking time to phase out
				assertNull(result);
			} catch(MalformedURLException murle) {
				fail(murle.getMessage());
			} catch(CoreException ce) {
				fail(ce.getMessage());
			}
		}, STATUS_200_OK, DEFAULT_DELAY);
	}
	@Test
	public void testPlatformUrl() {
		String urlString ="platform:/plugin/" + FoundationTestConstants.PLUGIN_ID + "/data/simpletext.txt"; 
		testDownload(urlString);
		testLastModified(urlString);
	}

	@Test
	public void testBundleEntryUrl() {
		Bundle b = Platform.getBundle(FoundationTestConstants.PLUGIN_ID);
		URL url = b.getEntry("data/simpletext.txt");
		testDownload(url.toExternalForm());
		testLastModified(url.toExternalForm());
	}

	
	// Test the caching preferences
	@Test
	public void testCaching() {
		// Simple test to verify caching is persisted
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID); 
		String CACHE_MAP_KEY = "URLTransportCache.CacheMapKey";  // internal key, but cannot change
		String val = prefs.get(CACHE_MAP_KEY, "");
		assertEquals(val, "");
		try {
			String urlString = "https://raw.github.com/jboss-jdf/jdf-stack/1.0.0.Final/stacks.yaml";
			boolean outdated = new URLTransportUtility().isCacheOutdated(urlString, new NullProgressMonitor());
			assertTrue(outdated);
			
			File downloaded = new URLTransportUtility().getCachedFileForURL(urlString, "stuff", URLTransportUtility.CACHE_FOREVER, new NullProgressMonitor());
			
			outdated = new URLTransportUtility().isCacheOutdated(urlString, new NullProgressMonitor());
			assertTrue(outdated);  // cache is outdated because github indicates remote file has timestamp of 0
			assertTrue(downloaded.exists());
		} catch(CoreException ce) {
			fail(ce.getMessage());
		} catch(Exception uee) {
			fail(uee.getMessage()); // should never happen
		}
	}

	private void executeWithHttpServer(ConsumerWithException consumer, int status, int delay) throws Exception {
		Server server = startFakeHttpServer(status, delay);
		try {
			consumer.accept(server);
		} finally {
			try {
				server.stop();
			} catch (Exception e) {}
		}		
	}

	@FunctionalInterface
	interface ConsumerWithException {
		void accept(Server server) throws Exception;
	}
	
}
