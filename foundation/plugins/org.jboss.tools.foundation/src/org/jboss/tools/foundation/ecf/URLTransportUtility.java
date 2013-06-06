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
package org.jboss.tools.foundation.ecf;

import java.io.File;
import java.io.OutputStream;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.jboss.tools.foundation.FoundationCorePlugin;
import org.jboss.tools.foundation.ecf.internal.InternalURLTransport;
import org.jboss.tools.foundation.ecf.internal.URLTransportCache;
import org.jboss.tools.foundation.jobs.BarrierProgressWaitJob;
import org.jboss.tools.foundation.jobs.BarrierProgressWaitJob.IRunnableWithProgress;

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
	 * Get a cached file for the given url. 
	 * If a cached version does not yet exist, download one now.
	 * 
	 * @param url
	 * @param displayName
	 * @param lifespan
	 * @param monitor
	 * @return
	 * @throws CoreException
	 */
	public File getCachedFileForURL(String url, String displayName, int lifespan,
			IProgressMonitor monitor) throws CoreException {
		monitor.beginTask(displayName, 200);
		IProgressMonitor sub1 = new SubProgressMonitor(monitor, 100);
		if( URLTransportCache.getDefault().isCacheOutdated(url, sub1)) {
			sub1.done();
			IProgressMonitor sub2 = new SubProgressMonitor(monitor, 100);
			return URLTransportCache.getDefault().downloadAndCache(url, 
					displayName, lifespan, this, sub2);
		} else {
			return URLTransportCache.getDefault().getCachedFile(url);
		}
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
		BarrierProgressWaitJob j = new BarrierProgressWaitJob("Check Remote URL Last Modified",  new IRunnableWithProgress() {
			public Object run(IProgressMonitor monitor) throws Exception {
				return getTransport().getLastModified(location, monitor);
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
	public IStatus download(final String displayName,final String url,final OutputStream destination, final int timeout, final IProgressMonitor monitor) {
		BarrierProgressWaitJob j = new BarrierProgressWaitJob("Download Remote URL",  new IRunnableWithProgress() {
			public Object run(IProgressMonitor monitor) throws Exception {
				return getTransport().download(displayName, url, destination, timeout, monitor);
			}
		});
		j.schedule();
		// This join will also poll the provided monitor for cancelations
		j.monitorSafeJoin(monitor);
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
