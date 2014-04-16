/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
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
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Random;
import java.util.concurrent.Executors;

import junit.framework.TestCase;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.core.test.FoundationTestConstants;
import org.jboss.tools.foundation.core.test.testutils.FakeHttpServer;
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
	
	
	protected FakeHttpServer startFakeSlowHttpServer(String statusLine) throws IOException {
		int port = new Random().nextInt(9 * 1024) + 1024;
		FakeHttpServer serverFake = null;
		String sLine = statusLine == null ? FakeHttpServer.DEFAULT_STATUSLINE : statusLine;
		serverFake = new FakeHttpServer(port, null, sLine) {

			public void start() throws IOException {
				executor = Executors.newFixedThreadPool(1);
				this.serverSocket = new ServerSocket(port);
				executor.submit(new ServerFakeSocket() { 
					protected String getResponse(Socket socket) throws IOException {
						System.out.println("Response requested. Delaying");
						try {
							Thread.sleep(45000);
						} catch(InterruptedException ie) {}
						System.out.println("Responding to request: Hello");
						return "Hello";
					}
				});
			}
		};
		serverFake.start();
		return serverFake;
	}
	
	@Test
	public void testTimeoutWithoutCancel() {
		FakeHttpServer server = null;
		try {
			server = startFakeSlowHttpServer(null);
		} catch(IOException ioe) {
			fail();
		}
		try {
			URL url = server.getUrl();
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
				server.stop();
		}

	}
	
	@Test
	public void testCancelMonitor() {
		FakeHttpServer server = null;
		try {
			server = startFakeSlowHttpServer(null);
		} catch(IOException ioe) {
			fail();
		}
		
		
		final Long[] times = new Long[2]; 	
		
		try {
			URL url = server.getUrl();
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
		} finally {
			if( server != null )
				server.stop();
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
			assertFalse(outdated);
			assertTrue(downloaded.exists());
		} catch(CoreException ce) {
			fail(ce.getMessage());
		} catch(Exception uee) {
			fail(uee.getMessage()); // should never happen
		}
	}
	
}
