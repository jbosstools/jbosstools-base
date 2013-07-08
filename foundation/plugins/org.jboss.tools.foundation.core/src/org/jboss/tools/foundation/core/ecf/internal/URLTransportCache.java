/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.ecf.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.security.SecureRandom;
import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.Trace;
import org.jboss.tools.foundation.core.ecf.Messages;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.osgi.service.prefs.BackingStoreException;

public class URLTransportCache {
	private static URLTransportCache singleton;
	/**
	 * The local caches will be stored in this plugin's
	 * .metadata/ECF_REMOTE_CACHE folder
	 */
	private static final String LOCAL_CACHE_LOCATION_FOLDER = "ECF_REMOTE_CACHE";
	private static final String CACHE_MAP_KEY = "URLTransportCache.CacheMapKey";

	public static URLTransportCache getDefault() {
		if (singleton == null)
			singleton = new URLTransportCache();
		return singleton;
	}

	private static HashMap<String, String> cache;

	public URLTransportCache() {
		cache = new HashMap<String, String>();
		loadPreferences();
	}

	/**
	 * Get a cached file for the given url only if it is downloaded and exists.
	 * 
	 * @param url
	 * @return
	 */
	public File getCachedFile(String url) {
		if (cache.get(url) == null)
			return null;
		File f = new File(cache.get(url));
		if (f.exists())
			return f;
		return null;
	}

	public boolean isCacheOutdated(String url, IProgressMonitor monitor)
			throws CoreException {
		Trace.trace(Trace.STRING_FINER, "Checking if cache is outdated for " + url);
		File f = getCachedFile(url);
		if (f == null)
			return true;

		URL url2 = null;
		try {
			url2 = new URL(url);
		} catch (MalformedURLException murle) {
			throw new CoreException(FoundationCorePlugin.statusFactory()
					.errorStatus(Messages.ECFExamplesTransport_IO_error, murle));
		}

		long remoteModified = new URLTransportUtility().getLastModified(url2, monitor);

		if (remoteModified == -1) {
			if (f.exists())
				return false;
		}
		// !!! urlModified == 0 when querying files from github
		// It means that files from github can not be cached!
		if (f.exists()) {
			long modified = f.lastModified();
			if (modified > 0 && // file already exists and doesn't come from
								// github (or other server sending lastmodified
								// = 0)
					(remoteModified == 0 // and now there is a problem
											// downloading the file
					|| remoteModified == modified)) {// or the file hasn't
														// changed
				return false;
			}
		}
		return true;
	}

	public File downloadAndCache(String url, String displayName, int lifespan,
			URLTransportUtility util, IProgressMonitor monitor) throws CoreException {
		Trace.trace(Trace.STRING_FINER, "Downloading and caching " + url + " with lifespan=" + lifespan);

		File target = getRemoteFileCacheLocation(url);
		try {
			OutputStream os = new FileOutputStream(target);
			IStatus s = util.download(displayName, url,
					os, monitor);
			if (s.isOK()) {
				if (lifespan == URLTransportUtility.CACHE_UNTIL_EXIT)
					target.deleteOnExit();
				addToCache(url, target);
			}
			return target != null && target.exists() ? target : null;
		} catch (IOException ioe) {
			throw new CoreException(FoundationCorePlugin.statusFactory()
					.errorStatus(Messages.ECFExamplesTransport_IO_error, ioe));
		}
	}

	private void addToCache(String url, File target) {
		cache.put(url, target.getAbsolutePath());
		savePreferences();
	}
	
	private void loadPreferences() {
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID); 
		String val = prefs.get(CACHE_MAP_KEY, "");
		if( !isEmpty(val)) {
			String[] byLine = val.split("\n");
			for( int i = 0; i < byLine.length; i++ ) {
				if( isEmpty(byLine[i]))
					continue;
				String[] kv = byLine[i].split("=");
				if( kv.length == 2 && !isEmpty(kv[0]) && !isEmpty(kv[1])) {
					if( new File(kv[1]).exists() )
						cache.put(kv[0],kv[1]);
				}
			}
		}
		Trace.trace(Trace.STRING_FINER, "Loaded " + cache.size() + " cache file locations from preferences");
	}
	
	private boolean isEmpty(String s) {
		return s == null || "".equals(s);
	}
	
	private void savePreferences() {
		// Save to prefs
		// saves plugin preferences at the workspace level
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID); 

		Trace.trace(Trace.STRING_FINER, "Saving " + cache.size() + " cache file locations to preferences");

		StringBuffer sb = new StringBuffer();
		Iterator<String> it = cache.keySet().iterator();
		while(it.hasNext()) {
			String k = it.next();
			String v = cache.get(k);
			String encodedURL = null;
			try {
				encodedURL = URLEncoder.encode(k, "UTF-8");
			} catch(UnsupportedEncodingException uee) {
				// Should never happen
			}
			if( encodedURL != null )
				sb.append(encodedURL + "=" + v + "\n");
		}

		prefs.put(CACHE_MAP_KEY, sb.toString());
		try {
			// prefs are automatically flushed during a plugin's "super.stop()".
			prefs.flush();
		} catch (BackingStoreException e) {
			FoundationCorePlugin.pluginLog().logError(e);
		}
	}

	/*
	 * Get a file in the core plugin's state location which is where the local
	 * cache of remote file would be
	 */
	private synchronized File getRemoteFileCacheLocation(String url) {
		// If this url is already cached, use it
		String cachedLoc = cache.get(url);
		if (cachedLoc != null) {
			File f = new File(cache.get(url));
			return f;
		}

		// Otherwise, make a new one
		IPath location = FoundationCorePlugin.getDefault().getStateLocation()
				.append(LOCAL_CACHE_LOCATION_FOLDER);
		File root = location.toFile();
		root.mkdirs();
		File cached = null;
		do {
			cached = new File(root, 
					"cache" + new SecureRandom().nextLong() + ".tmp");
		} while (cached.exists());

		return cached;
	}
}
