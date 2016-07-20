/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.credentials.internal;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;
import org.jboss.tools.foundation.ui.plugin.BaseUISharedImages;
import org.jboss.tools.magicfile4j.IMagicFileModel;
import org.jboss.tools.magicfile4j.MagicFileLoader;
import org.jboss.tools.magicfile4j.MagicResult;
import org.jboss.tools.magicfile4j.MagicRunner;
import org.osgi.framework.Bundle;

/**
 * A cache for downloaded favicon.ico files for given hosts. 
 * The images are stored in a {host} ->  {host}.ico  format in the map,
 * as well as in metadata locations. 
 * All requests for images should be made using the host alone as the key. 
 */
public class FaviconCache extends BaseUISharedImages {

	private static final FaviconCache instance = new FaviconCache(FoundationUIPlugin.getDefault().getBundle());
	
	public static FaviconCache getDefault() {
		return instance;
	}
	
	
	// A folder in state location to hold favicons
	private static final String FAVICONS = "favicons";
	private static final String DOT_ICO = ".ico";

	private static final String[] allowedFormats = new String[]{
			"image/vnd.microsoft.icon",
			"image/x-icon",
			"image/png",
			"image/gif",
			"image/jpeg",
			"image/svg+xml"
	};	
	private Set<String> failedLoads = new HashSet<String>();
	
	
	public FaviconCache(Bundle pluginBundle) {
		super(pluginBundle);
		init();
	}
	
	private void init() {
		// Load all already-downloaded icons
		IPath p = FoundationUIPlugin.getDefault().getStateLocation().append(FAVICONS);
		if( p.toFile().exists() ) {
			File[] all = p.toFile().listFiles();
			for( int i = 0; i < all.length; i++ ) {
				if( all[i].getName().endsWith(DOT_ICO)) {
					String host = all[i].getName().substring(0, all[i].getName().lastIndexOf("."));
					addImage(host, all[i].getName());
				}
			}
		} else {
			p.toFile().mkdirs();
		}

	}
	
	/**
	 * An interface to be alerted if a new favicon has been downloaded
	 */
	public static interface FaviconCacheListener {
		public void iconCached(String host);
		public void fetchFailed(String host);
	}
	
	/**
	 * An asynchronous method to load the favicon for a given host, 
	 * and alert a listener when complete
	 * @param host
	 * @param listener
	 */
	public void loadFavicon(final String host, final FaviconCacheListener listener) {
		new Thread("Load favicon.ico for " + host) {
			public void run() {
				loadFaviconSync(host, listener);
			}
		}.start();
	}
	
	private void loadFaviconSync(String host, FaviconCacheListener listener) {
		File f = fetchFaviconSync(host, listener);
		if( f == null ) {
			failedLoads.add(host);
			listener.fetchFailed(host);
		} else {
			String fileType = getMimeType(f);
			if( fileType == null || !Arrays.asList(allowedFormats).contains(fileType)) {
				// unapproved file type
				failedLoads.add(host);
				listener.fetchFailed(host);
			} else {
				// copy the file, and add it
				IPath p = FoundationUIPlugin.getDefault().getStateLocation().append(FAVICONS);
				String target = host+DOT_ICO;
				File targetFile = p.append(target).toFile();
				if( targetFile.exists()) {
					targetFile.delete();
				}
				boolean success = f.renameTo(targetFile);
				if( success ) {
					addImage(host,target);
					listener.iconCached(host);
				} else {
					failedLoads.add(host);
					listener.fetchFailed(host);
				}
			}
		}
	}
	
	private String getMimeType(File f) {
		if( f == null || !f.exists())
			return null;
		
		IMagicFileModel mod = getMagicModel();
		try {
			MagicResult result = new MagicRunner(mod).runMatcher(f);
			if( result != null ) {
				return result.getMimeType();
			}
		} catch(IOException ioe) {
			FoundationUIPlugin.pluginLog().logError("Error reading file type for file " + f.getName(), ioe);
		}
		return null;
	}
	
	private IMagicFileModel magicModel = null;
	private boolean magicLoadFailed = false;
	private IMagicFileModel getMagicModel() {
		if( magicModel == null && !magicLoadFailed) {
			URL url = null;
			InputStream inputStream = null;
			IMagicFileModel model = null;
			try {
				url = new URL("platform:/plugin/org.jboss.tools.foundation.ui/resources/images.magic");
			    inputStream = url.openConnection().getInputStream();
			    model = new MagicFileLoader().readMagicFile(inputStream);
			    magicModel = model;
			} catch (MalformedURLException e) {
				magicLoadFailed = true;
				FoundationUIPlugin.pluginLog().logError("Error loading magic file for verifying image types", e);
			} catch(IOException ioe) {
				magicLoadFailed = true;
				FoundationUIPlugin.pluginLog().logError("Error loading magic file for verifying image types", ioe);
			} finally {
				if (inputStream != null ) {
					try {
						inputStream.close();
					} catch(IOException ioe2) {
						// ignore
					}
				}
			}
		}
		return magicModel;
	}
	
	
	public boolean loadFailed(String host) {
		return failedLoads.contains(host);
	}
	
	private File fetchFaviconSync(String host, FaviconCacheListener listener) {
		try {
			String url = "http://" + host + "/favicon.ico";
			File f = new URLTransportUtility().getCachedFileForURL(url, "Fetching favicon for " + host, URLTransportUtility.CACHE_UNTIL_EXIT, new NullProgressMonitor());
			if( f != null && f.length() > 0)
				return f;
		} catch (CoreException ce) {
			FoundationUIPlugin.pluginLog().logError(ce);
		}
		return null;
	}
	

	public ImageDescriptor getDescriptorForHost(String host) {
		return descriptor(host);
	}
	
	public Image getImageForHost(String host) {
		try {
			return image(host);
		} catch(SWTException swte) {
			// ignore, just return no image
		}
		return null;
	}
	
	
	
	protected ImageDescriptor createImageDescriptor(String path) {
		try {
			IPath p = FoundationUIPlugin.getDefault().getStateLocation().append(FAVICONS);
			p = p.append(path);
			return ImageDescriptor.createFromURL(p.toFile().toURI().toURL());
		} catch(MalformedURLException murle) {
			FoundationUIPlugin.pluginLog().logError("Error generating favicon image descriptor for host " + path);
		}
		return null;
	}
	
	public static void cleanup() {
		FaviconCache.instance.dispose();
	}
}
