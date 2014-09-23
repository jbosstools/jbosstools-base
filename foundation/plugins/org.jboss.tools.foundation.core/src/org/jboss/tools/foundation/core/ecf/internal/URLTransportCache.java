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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
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

public class URLTransportCache {
	/**
	 * Encoding for this file
	 */
	private static final String ENCODING = "UTF-8";
	
	/**
	 * The default cache folder name inside foundation.core's metadata
	 */
	private static final String LOCAL_CACHE_LOCATION_FOLDER = "ECF_REMOTE_CACHE";
	
	/**
	 * The legacy key for pulling the cache map from plugin preferences
	 */
	private static final String CACHE_MAP_KEY = "URLTransportCache.CacheMapKey";

	
	/**
	 * The new index file, to be stored in each cache root folder. 
	 */
	private static final String CACHE_INDEX_FILE = "URLTransportCache.cacheIndex.properties";
	
	
	/**
	 * The default cache folder
	 */
	private static final IPath DEFAULT_CACHE_FOLDER = FoundationCorePlugin.getDefault().getStateLocation().append(LOCAL_CACHE_LOCATION_FOLDER);
	
	/**
	 * A collection of all caches currently in use. 
	 * This is to ensure multiple clients don't try using 
	 * two instances with the same basedir, which could lead to corruption of the cache.
	 */
	private static final HashMap<IPath, URLTransportCache> cacheDirToCache = new HashMap<IPath, URLTransportCache>();
	

	public synchronized static URLTransportCache getDefault() {
		return getCache(DEFAULT_CACHE_FOLDER);
	}

	public synchronized static URLTransportCache getCache(IPath root) {
		URLTransportCache c = cacheDirToCache.get(root);
		if( c == null ) {
			c = new URLTransportCache(root);
			cacheDirToCache.put(root, c);
		}
		return c;
	}
	
	
	private HashMap<String, String> cache;
	private IPath cacheRoot;
	protected URLTransportCache(IPath cacheRoot) {
		this.cacheRoot = cacheRoot;
		this.cache = new HashMap<String, String>();
		load();
	}

	/**
	 * Get a cached file for the given url only if it is downloaded and exists.
	 * 
	 * @param url
	 * @return
	 */
	public File getCachedFile(String url) {
		String cacheVal = cache.get(url);
		if (cacheVal == null)
			return null;
		File f = new File(cacheVal);
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
			if( remoteModified > modified ) {
				// The remote file has been updated *after* the local file was created, so, outdated
				return true;
			}
			if( remoteModified == 0 ) {
				// File comes from github or some other server not keeping accurate timestamps
				// so, possibly oudated, and must re-fetch 
				return true;
			}
			// Our local copy has a higher timestamp, so was fetched after
			return false;
		}
		// Local file doesn't exist, so, cache is outdated
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
	
	private void load() {
		if( cacheRoot.equals(DEFAULT_CACHE_FOLDER)) {
			// Load from the legacy preferences first. 
			// These values will be overridden by those in a concrete index file
			IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(FoundationCorePlugin.PLUGIN_ID); 
			String val = prefs.get(CACHE_MAP_KEY, "");
			loadIndexFromString(val);
		}
		
		File index = cacheRoot.append(CACHE_INDEX_FILE).toFile();
		if( index.exists() && index.isFile()) {
			try {
				String contents = getContents(index);
				loadIndexFromString(contents);
			} catch(IOException ioe) {
				FoundationCorePlugin.pluginLog().logError(ioe);
			}
		}
		Trace.trace(Trace.STRING_FINER, "Loaded " + cache.size() + " cache file locations from preferences");
	}
	
	private void loadIndexFromString(String val) {
		if( !isEmpty(val)) {
			String[] byLine = val.split("\n");
			for( int i = 0; i < byLine.length; i++ ) {
				if( isEmpty(byLine[i]))
					continue;
				String[] kv = byLine[i].split("=");
				if( kv.length == 2 && !isEmpty(kv[0]) && !isEmpty(kv[1])) {
					try {
						String decodedUrl = URLDecoder.decode(kv[0],ENCODING);
						if( new File(kv[1]).exists() )
							cache.put(decodedUrl,kv[1]);
					} catch(UnsupportedEncodingException uee) {
						// Should not be hit
						FoundationCorePlugin.pluginLog().logError(uee);
					}
				}
			}
		}
	}
	
	
	private boolean isEmpty(String s) {
		return s == null || "".equals(s);
	}
	
	private void savePreferences() {
		// Saves are now done to an index file in the cache root. 
		File index = cacheRoot.append(CACHE_INDEX_FILE).toFile();
		
		Trace.trace(Trace.STRING_FINER, "Saving " + cache.size() + " cache file locations to " + index.getAbsolutePath());

		StringBuffer sb = new StringBuffer();
		Iterator<String> it = cache.keySet().iterator();
		while(it.hasNext()) {
			String k = it.next();
			String v = cache.get(k);
			String encodedURL = null;
			try {
				encodedURL = URLEncoder.encode(k, ENCODING);
			} catch(UnsupportedEncodingException uee) {
				// Should never happen
			}
			if( encodedURL != null )
				sb.append(encodedURL + "=" + v + "\n");
		}

		try {
			setContents(index, sb.toString());
		} catch (IOException e) {
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
		File root = getLocalCacheFolder().toFile();
		root.mkdirs();
		
		String tmp = url.replaceAll("[\\p{Punct}&&[^_]]", "_");
		
		File cached = null;
		do {
			cached = new File(root, 
					tmp + new SecureRandom().nextLong() + ".tmp");
		} while (cached.exists());

		return cached;
	}
	
	private IPath getLocalCacheFolder() {
		return cacheRoot;
	}
	
	/* 
	 * foundation.core has no IO utility classes. 
	 * If it gets some, this should be saved there. 
	 */
	
	private static String getContents(File aFile) throws IOException {
		return new String(getBytesFromFile(aFile), ENCODING);
	}

	private static byte[] getBytesFromFile(File file) throws IOException {
        InputStream is = new FileInputStream(file);
        try {
	        byte[] bytes = new byte[(int)file.length()];
	        int offset = 0;
	        int numRead = 0;
	        while (offset < bytes.length
	               && (numRead=is.read(bytes, offset, bytes.length-offset)) >= 0) {
	            offset += numRead;
	        }
	        return bytes;
        } finally {
            is.close();
        }
    }
	
	private static void setContents(File file, String contents) throws IOException {
		Writer out = null;
		try {
			out = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(file), ENCODING));
			out.append(contents);
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}
}
