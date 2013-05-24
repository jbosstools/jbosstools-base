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
import java.net.URI;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.common.core.CommonCorePlugin;

public class ECFTransportUtility {
	public static final String PROTOCOL_FILE = "file"; //$NON-NLS-1$
	public static final String PROTOCOL_PLATFORM = "platform"; //$NON-NLS-1$
	
	
	/**
	 * Need to javadoc this method as it's not clear 
	 * what the args are from a quick read. 
	 * 
	 * @param url The URL to pull from
	 * @param prefix the filename prefix (for stacks.yaml, this would be 'stacks')
	 * @param suffix the filename suffix (for stacks.yaml, this would be 'yaml')
	 * @param monitor A progress Monitor
	 * @return
	 */
	public static File getFileFromURL(URL url, String prefix,
			String suffix, IProgressMonitor monitor) {
		File file = null;
		if (PROTOCOL_FILE.equals(url.getProtocol())
				|| PROTOCOL_PLATFORM.equalsIgnoreCase(url.getProtocol())) {
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
		} else {
			try {
				if (monitor.isCanceled()) {
					return null;
				}
				long urlModified = -1;
				file = getFile(url);
				try {
					urlModified = getTransport().getLastModified(url);
				} catch (CoreException e) {
					if (file.exists()) {
						return file;
					}
				}
				//!!! urlModified == 0 when querying files from github 
				//It means that files from github can not be cached! 
				if (file.exists()) {
					long modified = file.lastModified();
					if (modified > 0 && //file already exists and doesn't come from github (or other server sending lastmodified = 0) 
							(urlModified == 0 //and now there is a problem downloading the file
							|| urlModified == modified )) {//or the file hasn't changed
						return file;
					}
				}
				// file = File.createTempFile(prefix, suffix);
				// file.deleteOnExit();
				file.getParentFile().mkdirs();
				if (monitor.isCanceled()) {
					return null;
				}
				BufferedOutputStream destination = new BufferedOutputStream(
						new FileOutputStream(file));
				IStatus result = getTransport().download(prefix,
						url.toExternalForm(), destination, monitor);
				if (!result.isOK()) {
					CommonCorePlugin.getDefault().getLog().log(result);
					return null;
				} else {
					if (file.exists()) {
						file.setLastModified(urlModified);
					}
				}
			} catch (FileNotFoundException e) {
				CommonCorePlugin.logException(e);
				return null;
			}
		}
		return file;
	}
	
	private static ECFTransport getTransport() {
		return ECFTransport.getInstance();
	}
	
	private static File getFile(URL url) {
		IPath location = CommonCorePlugin.getDefault().getStateLocation();
		File root = location.toFile();
		String urlFile = url.getFile();
		File file = new File(root, urlFile);
		return file;
	}

}
