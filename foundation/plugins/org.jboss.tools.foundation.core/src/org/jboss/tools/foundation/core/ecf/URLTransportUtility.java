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
package org.jboss.tools.foundation.core.ecf;

import java.io.File;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.ecf.internal.InternalURLTransport;
import org.jboss.tools.foundation.core.ecf.internal.URLTransportCache;
import org.jboss.tools.foundation.core.jobs.BarrierProgressWaitJob;
import org.jboss.tools.foundation.core.jobs.BarrierProgressWaitJob.IRunnableWithProgress;

/**
 * This class is intended to be used to perform work 
 * associated with remote files for a variety of tasks using ECF, 
 * including: 
 *    - fetch URL's (local files, eclipse-specific urls, or web urls)
 *    - check remote timestamp of such urls
 *    - store fetched urls in a local cache for repeated use
 * 
 * @noextend This class is not intended to be subclassed by clients.
 */
public class URLTransportUtility {
	/**
	 * When using the caching signature, getCachedFileForURL(etc), 
	 * this indicates the file should remain around cached indefinitely.
	 */
	public static final int CACHE_FOREVER = 1;
	
	/**
	 * When using the caching signature, getCachedFileForURL(etc), 
	 * this indicates the file should be cached and available until 
	 * the eclipse session ends.
	 */
	public static final int CACHE_UNTIL_EXIT = 2;


	/**
	 * Get the cached file, or download a new copy if the local cache is outdated or does not exist. 
	 * The default cache folder will be used. 
	 * 
	 * @param url			The URL
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param monitor   	Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */
	public File getCachedFileForURL(String url, String displayName, int lifespan,
			IProgressMonitor monitor) throws CoreException {
		return getCachedFileForURL(url, displayName, lifespan, URLTransportCache.getDefault(), monitor);
	}
	
	/**
	 * Get the cached file, or download a new copy if the local cache is outdated or does not exist. 
	 * The default connection timeouts for your system will be used.
	 * 
	 * @param url			The URL
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param timeout  		The timeout in ms to be passed to the connection. 
	 * 						A value of less than 0  will NOT pass in or override the timeouts.  
	 * @param monitor 		Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */

	public File getCachedFileForURL(String url, String displayName, int lifespan,
			int timeout, IProgressMonitor monitor) throws CoreException {
		return getCachedFileForURL(url, displayName, lifespan, URLTransportCache.getDefault(), timeout, monitor);
	}

	/**
	 * Get the cached file, or download a new copy if the local cache is outdated or does not exist. 
	 * The default connection timeouts for your system will be used.
	 * 
	 * @param url			The URL
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param cacheRoot		A path to a folder to use as a cache
	 * @param monitor	  	Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */
	public File getCachedFileForURL(String url, String displayName, int lifespan,
			IPath cacheRoot, IProgressMonitor monitor) throws CoreException {
		URLTransportCache cache = URLTransportCache.getCache(cacheRoot);
		return getCachedFileForURL(url, displayName, lifespan, cache, monitor);
	}
	

	/**
	 * Get the cached file, or download a new copy if the local cache is outdated or does not exist. 
	 * 
	 * @param url			The URL
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param cacheRoot		A path to a folder to use as a cache
	 * @param timeout  		The timeout in ms to be passed to the connection. 
	 * 						A value of less than 0  will NOT pass in or override the timeouts.  
	 * @param monitor	  	Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */

	public File getCachedFileForURL(String url, String displayName, int lifespan,
			IPath cacheRoot, int timeout, IProgressMonitor monitor) throws CoreException {
		URLTransportCache cache = URLTransportCache.getCache(cacheRoot);
		return getCachedFileForURL(url, displayName, lifespan, cache, timeout, monitor);
	}

	
	
	public boolean isCacheOutdated(String url, IProgressMonitor mon) throws CoreException {
		return isCacheOutdated(url, URLTransportCache.getDefault(), mon);
	}
	
	public boolean isCacheOutdated(String url, IPath cacheRoot, IProgressMonitor mon) throws CoreException {
		return isCacheOutdated(url, URLTransportCache.getCache(cacheRoot), mon);
	}

	public boolean isCacheOutdated(String url, URLTransportCache cache, IProgressMonitor mon) throws CoreException {
		return cache.isCacheOutdated(url, mon);
	}

	
	/**
	 * Get the cached file, or download a new copy if the local cache is outdated. 
	 * Timeouts will use the default values for your system
	 * 
	 * @param url			The URL
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param cache			A transport cache instance
	 * @param monitor	  	Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */
	private File getCachedFileForURL(String url, String displayName, int lifespan,
			URLTransportCache cache, IProgressMonitor monitor) throws CoreException {
		return getCachedFileForURL(url, displayName, lifespan, cache, -1, monitor);
	}
	
	/**
	 * Get the cached file, or download a new copy if the local cache is outdated. 
	 * Timeouts will use the default values for your system
	 * 
	 * @param url			The URL
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param cache			A transport cache instance
	 * @param timeout  		The timeout in ms to be passed to the connection. 
	 * 						A value of less than 0  will NOT pass in or override the timeouts.  
	 * @param monitor	  	Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */
	private File getCachedFileForURL(String url, String displayName, int lifespan,
			URLTransportCache cache, int timeout, IProgressMonitor monitor) throws CoreException {

		monitor.beginTask(displayName, 200);
		IProgressMonitor sub1 = new SubProgressMonitor(monitor, 100);
		try {
			if( cache.isCacheOutdated(url, sub1)) {
				sub1.done();
				// If the remote cache is outdated, fetch the new copy
				IProgressMonitor sub2 = new SubProgressMonitor(monitor, 100);
				return cache.downloadAndCache(url, displayName, lifespan, this, timeout, sub2);
			} else {
				// Else use the local cache
				return cache.getCachedFile(url);
			}
		} catch(CoreException ce) {
			// We cannot reach the remote url. If there's a local cache copy, use that
			File f = cache.getCachedFile(url);
			if( f != null && f.exists())
				return f;
			// If not, re-throw the CoreException
			IStatus old = ce.getStatus();
			IStatus rethrow = new Status(old.getSeverity(), old.getPlugin(), old.getMessage(), ce);
			throw new CoreException(rethrow);
		}
	}
	
	/**
	 * Get the cached file, or download a new copy if the local cache is outdated. 
	 * 
	 * This signature is intended for use when supplying a time-to-live duration
	 * to ensure a speedy return after a time limit has been exceeded. 
	 * 
	 * @param uri			The URI
	 * @param displayName	A user-visible string for this task
	 * @param lifespan		How long the download file should be kept: one of CACHE_FOREVER or CACHE_UNTIL_EXIT
	 * @param transport		The transport utility
	 * @param timeout  		The timeout in ms to be passed to the connection. 
	 * 						A value of less than 0  will NOT pass in or override the timeouts.  
	 * @param timeToLive	A positive long duration in ms for this execution to live. 
	 *						Negative or 0 will be treated as infinite
	 * @param monitor	  	Progress monitor
	 * @return 				A file representing the cached or newly cached file, or null if none exists
	 * @throws CoreException  If a download failed, was canceled, or unexpected error occurred
	 */
	  public File getCachedFileForURL(String uri, String displayName, int lifespan, int timeout, 
			  final long timeToLive, final IProgressMonitor monitor) throws CoreException {
		  
		    //We time-bomb the monitor in case resolving/downloading is stuck at the OS level, 
		    //i.e. http connection timeout would be ignored
		    final Boolean[] returned = new Boolean[1];
		    returned[0] = false;
		    
		    if( timeToLive > 0 ) {
				new Thread() {	
					public void run() {
						try {
							Thread.sleep(timeToLive);
						} catch(InterruptedException ie) {}
						// If the method still hasn't returned, cancel the monitor and abort
						if (!returned[0]) {
							monitor.setCanceled(true);
						}
					}
				}.start();  
		    }
		    
		    // Updated to pass a timeout 
		    File propFile = getCachedFileForURL( uri, displayName, lifespan, timeout, monitor);
		    returned[0] = true;
		    return propFile;
	  } 
	/**
	 * Get the last modified timestamp of a given URL
	 * @param location
	 * @return
	 * @throws CoreException
	 */
	public long getLastModified(URL location) throws CoreException {
		return getLastModified(location, new NullProgressMonitor());
	}
	
	/**
	 * Get the last modified timestamp of a given URL
	 * @param location
	 * @param timeout
	 * @return
	 * @throws CoreException
	 */
	public long getLastModified(final URL location, IProgressMonitor monitor) throws CoreException {
		return getLastModified(location, null, null, monitor);
	}
	
	/**
	 * Get the last modified timestamp of a given URL
	 * @param location
	 * @param timeout
	 * @return
	 * @throws CoreException
	 */
	public long getLastModified(final URL location, final String user, final String pass, IProgressMonitor monitor) throws CoreException {
		BarrierProgressWaitJob j = new BarrierProgressWaitJob("Check Remote URL Last Modified",  new IRunnableWithProgress() {
			public Object run(IProgressMonitor monitor) throws Exception {
				return getTransport().getLastModified(location, user, pass, monitor);
			}
		});
		j.schedule();
		// This join will also poll the provided monitor for cancelations
		j.monitorSafeJoin(monitor);
		if( monitor.isCanceled())
			return -1;
		if( j.getThrowable() != null ) {
			if( j.getThrowable() instanceof CoreException)
				throw (CoreException)j.getThrowable();
			throw new RuntimeException(j.getThrowable());
		}
		return ((Long)j.getReturnValue()).longValue();
	}


	/**
	 * 
	 * @param displayName A string representation of the URL suitable for display / progress purposes
	 * @param url  The URL to fetch
	 * @param destination  The output stream to feed the contents to
	 * @param monitor A progress monitor
	 * @return A status object indicating the success or failure
	 */
	public IStatus download(String displayName, String url, OutputStream destination, IProgressMonitor monitor) {
		return download(displayName, url, destination, -1, monitor);
	}

	/**
	 * Download the given file to the given output stream
	 * 
	 * @param displayName A string representation of the URL suitable for display / progress purposes
	 * @param url  The URL to fetch
	 * @param destination  The output stream to feed the contents to
	 * @param timeout a timeout duration for how long connections should attempt to connect
	 * @param monitor A progress monitor
	 * @return A status object indicating the success or failure
	 */
	public IStatus download(final String displayName,final String url,final OutputStream destination, final int timeout, final IProgressMonitor monitor2) {
		return download(displayName, url, null, null, destination, timeout, monitor2);
	}
	
	public IStatus download(final String displayName,final String url, final String user, final String pass, final OutputStream destination, final int timeout, final IProgressMonitor monitor2) {
		BarrierProgressWaitJob j = new BarrierProgressWaitJob("Download Remote URL",  new IRunnableWithProgress() {
			public Object run(IProgressMonitor monitor) throws Exception {
				return getTransport().download(displayName, url, user, pass, destination, timeout, monitor2);
			}
		});
		j.schedule();
		// This join will also poll the provided monitor for cancelations
		j.monitorSafeJoin(monitor2);
		if( j.getReturnValue() != null)
			return (IStatus)j.getReturnValue();
		if( j.getThrowable() != null ) {
			throw new RuntimeException(j.getThrowable());
		}
		return FoundationCorePlugin.statusFactory().cancelStatus(Messages.ECFTransport_Operation_canceled);
	}
	
	/*
	 * Get the internal transport object
	 */
	private static InternalURLTransport getTransport() {
		return InternalURLTransport.getInstance();
	}
}
