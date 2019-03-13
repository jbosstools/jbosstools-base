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
import java.net.URL;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import junit.framework.TestCase;

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
import org.junit.Test;
import org.osgi.framework.Bundle;

public class URLTransportUtilTest extends TestCase {
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
	
	public void testLongUrl() throws Exception {
		Server server = startFakeHttpServer(200, 0);
		try {
			  String urlString = server.getURI() + "/wearejustdoingthistobestupidnowsincethiscangoonforeverandeverandeverbutitstilllookskindaneatinthebrowsereventhoughitsabigwasteoftimeandenergyandhasnorealpointbutwehadtodoitanyways.html";
			  File file = new URLTransportUtility().getCachedFileForURL(urlString, "Long Url", URLTransportUtility.CACHE_UNTIL_EXIT, new NullProgressMonitor());
			  assertTrue(file.exists());
			  assertTrue(file.length() > 0);
		} finally {
			try {
				server.stop();
			} catch (Exception e) {}
		}
	}

	protected Server startFakeSlowHttpServer() throws Exception {
		return startFakeHttpServer(200, 45000);
	}
	
	protected Server startFakeSlowHttpServer(int status) throws Exception {
		return startFakeHttpServer(status, 45000);
	}
	
	protected Server startFakeHttpServer(int status, final long delay) throws Exception {
		Server server = new Server(0);
		server.setHandler(new AbstractHandler() {

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
			
		});
		server.start();
		return server;
	}
	
	@Test
	public void testTimeoutWithoutCancel() {
		Server server = null;
		try {
			server = startFakeSlowHttpServer();
		} catch(Exception ioe) {
			fail();
		}
		try {
			URL url = server.getURI().toURL();
			final IProgressMonitor monitor = new NullProgressMonitor();
			URLTransportUtility util = new URLTransportUtility();
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			long t1 = System.currentTimeMillis();
			IStatus s = util.download("displayString", url.toExternalForm(), os, 500, monitor);
			long t2 = System.currentTimeMillis();
			assertTrue(s.getSeverity() == IStatus.ERROR);
			assertTrue(t2 - t1 < 3000);
		} catch(MalformedURLException murle) {
		} finally {
			if( server != null )
				try {
					server.stop();
				} catch (Exception e) {}
		}

	}
	
	@Test
	public void testCancelMonitor() {
		Server server = null;
		try {
			server = startFakeSlowHttpServer();
		} catch(Exception ioe) {
			fail();
		}
		
		
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
		} finally {
			if( server != null )
				try {
					server.stop();
				} catch (Exception e) {}
		}
	}

	@Test
	public void testTimeToLive1() {
		testTimeToLive(1000);
	}
	
	@Test
	public void testTimeToLive2() {
		testTimeToLive(5000);
	}

	
	private void testTimeToLive(long timeout) {
		Server server = null;
		try {
			server = startFakeSlowHttpServer();
		} catch(Exception ioe) {
			fail();
		}
		
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
		} finally {
			if( server != null )
				try {
					server.stop();
				} catch (Exception e) {}
		}
		
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
	
}
