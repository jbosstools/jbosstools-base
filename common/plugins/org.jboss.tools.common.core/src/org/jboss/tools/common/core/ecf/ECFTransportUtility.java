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
package org.jboss.tools.common.core.ecf;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.net.ProtocolException;
import java.net.URI;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ecf.core.security.IConnectContext;
import org.eclipse.ecf.filetransfer.UserCancelledException;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.core.ecf.internal.InternalECFTransport;

public class ECFTransportUtility {
	private static final String PROTOCOL_FILE = "file"; //$NON-NLS-1$
	private static final String PROTOCOL_PLATFORM = "platform"; //$NON-NLS-1$
	
	/**
	 * The local caches will be stored in this plugin's .metadata/ECF_REMOTE_CACHE folder
	 */
	private static final String LOCAL_CACHE_LOCATION_FOLDER = "ECF_REMOTE_CACHE";
	
	
	/**
	 * This method is deprecated at the day of its creation.
	 * This means nobody should use this at all. 
	 * 
	 * It is only here for migration purposes until appropriate decisions can be 
	 * made for how to deprecate ECFTransport in o.j.t.runtimes
	 */
	@Deprecated
	public IConnectContext getConnectionContext(String xmlLocation, boolean prompt) throws UserCancelledException, CoreException {
		return getTransport().getConnectionContext(xmlLocation, prompt);
	}
	
	/**
	 * This method is deprecated at the day of its creation.
	 * This means nobody should use this at all. 
	 * 
	 * It is only here for migration purposes until appropriate decisions can be 
	 * made for how to deprecate ECFTransport in o.j.t.runtimes
	 */
	@Deprecated
	public IStatus performDownload(String name,String toDownload, OutputStream target, IConnectContext context, IProgressMonitor monitor) throws ProtocolException {
		return getTransport().performDownload(name, toDownload, target, context, monitor);
	}
	
	/**
	 * Get the last modified timestamp of a given URL
	 * @param location
	 * @return
	 * @throws CoreException
	 */
	public long getLastModified(URL location) throws CoreException {
		return getTransport().getLastModified(location);
	}
	
	/**
	 * 
	 * @param name A string representation of the URL suitable for display / progress purposes
	 * @param url  The URL to fetch
	 * @param destination  The output stream to feed the contents to
	 * @param monitor A progress monitor
	 * @return A status object indicating the success or failure
	 */
	public IStatus download(String name, String url, OutputStream destination, IProgressMonitor monitor) {
		return getTransport().download(name, url, destination, monitor);
	}
	
	
	public static File getFileFromURL(URL url, String prefix, String suffix, IProgressMonitor monitor) {
		return new ECFTransportUtility().getFileFromURL(url, prefix + "." + suffix, monitor);
	}

	
	/**
	 * fetch a file either from a remote URL, a local cache of 
	 * the remote URL, or, a local URL which is easily convertable to a file.
	 * 
	 * This file will stay around *forever* so it is the callers responsibility
	 * to remove it at a reasonable time.
	 * 
	 * Use the download(...) method with a proper outputstream
	 * for files that are seldom accessed and should be fetched every time. 
	 * 
	 * @param url The URL to pull from
	 * @param resourceName A string representation of the resource suitable for display
	 * @param monitor A progress Monitor
	 * @return
	 */
	public File getFileFromURL(URL url, String resourceName, IProgressMonitor monitor) {
		File file = null;
		if (PROTOCOL_FILE.equals(url.getProtocol())
				|| PROTOCOL_PLATFORM.equalsIgnoreCase(url.getProtocol())) {
			return getFileFromLocalURL(url);
		} else {
			if (monitor.isCanceled()) {
				return null;
			}
			file = getRemoteFileCacheLocation(url);
			long remoteModified = getRemoteTimestamp(url);
			boolean outdated = isLocalCacheOutdated(remoteModified, file);
			if( !outdated )
				return file;
			
			file.getParentFile().mkdirs();
			if (monitor.isCanceled()) {
				return null;
			}
			
			downloadToLocalCache(file, url, resourceName, remoteModified, monitor);
		}
		return file.exists() ? file : null;
	}

	private void downloadToLocalCache(File file, URL url, String resourceName, 
			long remoteModified, IProgressMonitor monitor) {

		try {
			BufferedOutputStream destination = new BufferedOutputStream(
					new FileOutputStream(file));
			IStatus result = download(resourceName, url.toExternalForm(), 
					destination, monitor);
			if (!result.isOK()) {
				CommonCorePlugin.getDefault().getLog().log(result);
			} else {
				if (file.exists()) {
					file.setLastModified(remoteModified);
				}
			}
		} catch(FileNotFoundException nfe) {
			// Ignore, handle in calling method
		}
	}
	
	/*
	 * Fetch the remote timestamp, or -1 if not accessible
	 */
	private long getRemoteTimestamp(URL url) {
		long urlModified = -1;
		try {
			urlModified = getTransport().getLastModified(url);
		} catch (CoreException e) {
			// IGNORE
		}
		return urlModified;
	}
	
	private boolean isLocalCacheOutdated(long remoteModified, File file) {
		if( remoteModified == -1 ) {
			if( file.exists() )
				return false;
			return true;
		}
		//!!! urlModified == 0 when querying files from github 
		//It means that files from github can not be cached! 
		if (file.exists()) {
			long modified = file.lastModified();
			if (modified > 0 && //file already exists and doesn't come from github (or other server sending lastmodified = 0) 
					(remoteModified == 0 //and now there is a problem downloading the file
					|| remoteModified == modified )) {//or the file hasn't changed
				return false;
			}
		}
		return true;
	}
	
	private File getFileFromLocalURL(URL url) {
		File file = null;
		try {
			// assume all illegal characters have been properly encoded, so
			// use URI class to unencode
			file = new File(new URI(url.toExternalForm()));
		} catch (Exception e) {
			// URL contains unencoded characters
			file = new File(url.getFile());
		}
		if (!file.exists())
			return null;
		return file;
	}
	
	
	/*
	 * Get the transport object
	 */
	private static InternalECFTransport getTransport() {
		return InternalECFTransport.getInstance();
	}
	
	/*
	 * Get a file in the core plugin's state location 
	 * which is where the local cache of remote file would be
	 */
	private static File getRemoteFileCacheLocation(URL url) {
		IPath location = CommonCorePlugin.getDefault().getStateLocation().append(LOCAL_CACHE_LOCATION_FOLDER);
		File root = location.toFile();
		String urlFile = url.getFile();
		File file = new File(root, urlFile);
		return file;
	}

}
