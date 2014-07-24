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
package org.jboss.tools.stacks.core.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.jboss.developer.stacks.client.DefaultStacksClientConfiguration;
import org.jboss.developer.stacks.client.StacksClient;
import org.jboss.developer.stacks.client.StacksClientConfiguration;
import org.jboss.developer.stacks.client.messages.StacksMessages;
import org.jboss.developer.stacks.model.Stacks;
import org.jboss.developer.stacks.parser.Parser;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.core.jobs.BarrierProgressWaitJob;
import org.jboss.tools.foundation.core.jobs.BarrierProgressWaitJob.IRunnableWithProgress;
import org.jboss.tools.stacks.core.StacksCoreActivator;
import org.jboss.tools.stacks.core.Trace;

/**
 * A StacksManager is in charge of retrieving a file from a URL or standard location
 * and returning a jdf.stacks model object generated via the stacks client. 
 */
public class StacksManager {

	/**
	 * @deprecated
	 */
	private static final String STACKS_URL_PROPERTY = "org.jboss.examples.stacks.url";
	
	private static final String URL_PROPERTY_STACKS = "org.jboss.tools.stacks.url_stacks";
	private static final String URL_PROPERTY_PRESTACKS = "org.jboss.tools.stacks.url_prestacks";
	
	
	private static final String STACKS_URL;
	private static final String PRESTACKS_URL;
	
	// Declare the types of stacks available for fetch
	public enum StacksType {
		STACKS_TYPE, PRESTACKS_TYPE
	}
	
	// Load the default stacks url and prestacks url from a sysprop or jar
	static {
		STACKS_URL = System.getProperty(URL_PROPERTY_STACKS, System.getProperty(STACKS_URL_PROPERTY, 
				System.getProperty(StacksClientConfiguration.REPO_PROPERTY, getStacksUrlFromJar())));
		// should use StacksCC prestacks property but that requires update of library. can be done after GA
		PRESTACKS_URL = System.getProperty(URL_PROPERTY_PRESTACKS, System.getProperty("jdf.prestacks.client.repo", getPreStacksUrlFromJar())); //$NON-NLS-1$ 
	}
	
	/**
	 * Fetch the default stacks model. 
	 * 
	 * @param monitor
	 * @return
	 */
	public Stacks getStacks(IProgressMonitor monitor) {
		Stacks[] all = getStacks("Fetching JBoss Stacks", monitor, StacksType.STACKS_TYPE);
		if( all != null && all.length > 0) 
			return all[0];
		return null;
	}
	
	/**
	 * Fetch an array of stacks models where each element represents one of the StacksType urls
	 * 
	 * @param jobName
	 * @param monitor
	 * @param types
	 * @return
	 */
	public Stacks[] getStacks(String jobName, IProgressMonitor monitor, StacksType... types) {
		if( types == null )
			return new Stacks[0];
		Trace.trace(Trace.STRING_FINEST, "Request received for " + types.length + " stacks types.");
		ArrayList<Stacks> ret = new ArrayList<Stacks>(types.length);
		monitor.beginTask(jobName, types.length * 100);
		for( int i = 0; i < types.length; i++ ) {
			switch(types[i] ) {
			case STACKS_TYPE:
				Trace.trace(Trace.STRING_FINEST, "Loading Stacks Model from " + STACKS_URL);
				Stacks s = getStacks(STACKS_URL, jobName, new SubProgressMonitor(monitor, 50));
				if( s == null && !monitor.isCanceled()) {
					StacksCoreActivator.pluginLog().logWarning("Stacks from "+ STACKS_URL +" can not be read, using client mechanism instead");
					s = getDefaultStacksFromClient(new SubProgressMonitor(monitor, 50));
				}
				ret.add(s);
				break;
			case PRESTACKS_TYPE:
				// Pre-stacks has no fall-back mechanism at this time
				Trace.trace(Trace.STRING_FINEST, "Loading Stacks Model from " + PRESTACKS_URL);
				ret.add(getStacks(PRESTACKS_URL, jobName, new SubProgressMonitor(monitor, 100)));
				break;
			default:
				break;
			}
		}
		monitor.done();
		return (Stacks[]) ret.toArray(new Stacks[ret.size()]);
	}
	
	
	/**
	 * Fetch the stacks model representing a given arbitrary url. 
	 * The remote file will be cached only until the system exits. 
	 * 
	 * @param url
	 * @param monitor
	 * @return
	 */
	public Stacks getStacks(String url, IProgressMonitor monitor) {
		return getStacks(url, url, URLTransportUtility.CACHE_UNTIL_EXIT, monitor);
	}
	
	/**
	 * Fetch the stacks model for a given url. Cache the remote file 
	 * with a duration representing forever, or, until the remote file is newer. 
	 * 
	 * @param url  The url
	 * @param jobName Job name for display purposes
	 * @param monitor
	 * @return
	 */
	protected Stacks getStacks(String url, String jobName, IProgressMonitor monitor) {
		return getStacks(url, jobName, URLTransportUtility.CACHE_FOREVER, monitor);
	}
	
	/**
	 * Fetch the stacks model for the given url. 
	 * 
	 * @param url
	 * @param jobName
	 * @param cacheType
	 * @param monitor
	 * @return
	 */
	protected Stacks getStacks(String url, String jobName, int cacheType, IProgressMonitor monitor) {
		return getStacksFromURL(url, jobName, cacheType, monitor);
	}
	
	protected Stacks getStacksFromURL(String url, String jobName, int cacheType, IProgressMonitor monitor) {
	
		Stacks stacks = null;
		try {
			Trace.trace(Trace.STRING_FINEST, "Locating or downloading file for " + url);
			File f = getCachedFileForURL(url, jobName, cacheType, monitor);
			return getStacksFromFile(f);
		} catch (Exception e) {
			StacksCoreActivator.pluginLog().logError("Can't access or parse  " + url, e ); //$NON-NLS-1$
		}
		return stacks;
	}
	
	protected Stacks getStacksFromFile(File f) throws IOException {
		if (f != null && f.exists()) {
			Trace.trace(Trace.STRING_FINEST, "Local file for url exists");
			FileInputStream fis = null;
			try {
				fis = new FileInputStream(f);
				Parser p = new Parser();
				return p.parse(fis);
			} finally {
				close(fis);
			}
		}
		return null;
	}
	
	private Stacks getDefaultStacksFromClient(IProgressMonitor monitor) {
		if (!monitor.isCanceled()) {
			final StacksClient client = new StacksClient(new DefaultStacksClientConfiguration(), new JBTStacksMessages());
			IRunnableWithProgress barrierRunnable = new IRunnableWithProgress() {
				public Object run(IProgressMonitor monitor) throws Exception {
					StacksCoreActivator.pluginLog().logWarning("BarrierProgressWaitJob - loading Stacks Client values");
					return client.getStacks();
				}
			};
			BarrierProgressWaitJob fromClientJob = new BarrierProgressWaitJob("Load stacks using stacks client", barrierRunnable);
			fromClientJob.schedule();
			fromClientJob.monitorSafeJoin(monitor);
			Throwable t = fromClientJob.getThrowable();
			Object ret = fromClientJob.getReturnValue();
			if( t != null ) {
				StacksCoreActivator.pluginLog().logError(t);
			}
			return (Stacks)ret;
		}
		return null;
	}
	
	
	/**
	 * Fetch a local cache of the remote file. 
	 * If the remote file is newer than the local, update it. 
	 * 
	 * @param url
	 * @param jobName
	 * @param monitor
	 * @return
	 */
	protected File getCachedFileForURL(String url, String jobName, IProgressMonitor monitor) throws CoreException {
		 return getCachedFileForURL(url, jobName, URLTransportUtility.CACHE_FOREVER, monitor);
	}
	
	/**
	 * Fetch a local cache of the remote file. 
	 * If the remote file is newer than the local, update it. 
	 * 
	 * @param url   A url to fetch the stacks model from
	 * @param jobName  A job name passed into the downloader for display purposes
	 * @param cacheType  An integer representing the duration to cache the downloaded file
	 * @param monitor A file representign the model
	 * @return
	 */
	protected File getCachedFileForURL(String url, String jobName, int cacheType, IProgressMonitor monitor) throws CoreException {
		 return new URLTransportUtility().getCachedFileForURL(url, jobName, cacheType, monitor);
	}

	
	/*
	 * Read the stacks.yaml location from inside our client jar
	 */
	private static String getStacksUrlFromJar() {
		InputStream is = null;
		try {
			is = StacksManager.class.getResourceAsStream("/org/jboss/developer/stacks/client/config.properties"); //$NON-NLS-1$
			Properties p = new Properties();
			p.load(is);
			return p.getProperty(StacksClientConfiguration.REPO_PROPERTY);
		} catch (Exception e) {
			StacksCoreActivator.pluginLog().logWarning("Can't read stacks url from the stacks-client.jar", e); //$NON-NLS-1$
		} finally {
			close(is);
		}
		return null;
	}
	
	private static String getPreStacksUrlFromJar() {
		InputStream is = null;
		try {
			is = StacksManager.class.getResourceAsStream("/org/jboss/developer/stacks/client/config.properties"); //$NON-NLS-1$
			Properties p = new Properties();
			p.load(is);
			return p.getProperty(StacksClientConfiguration.PRESTACKS_REPO_PROPERTY);
		} catch (Exception e) {
			StacksCoreActivator.pluginLog().logWarning("Can't read stacks url from the stacks-client.jar", e); //$NON-NLS-1$
		} finally {
			close(is);
		}
		return null;
	}

	
	private static class JBTStacksMessages implements StacksMessages {
		public void showDebugMessage(String arg0) {
			Trace.trace(Trace.STRING_FINER, arg0);
		}
		public void showInfoMessage(String arg0) {
			Trace.trace(Trace.STRING_INFO, arg0);
		}
		public void showErrorMessage(String arg0) {
			StacksCoreActivator.pluginLog().logError(arg0);
		}
		public void showErrorMessageWithCause(String arg0, Throwable t) {
			StacksCoreActivator.pluginLog().logError(arg0, t);
		}
		public void showWarnMessage(String arg0) {
			StacksCoreActivator.pluginLog().logWarning(arg0);
		}
		
	}
	
	/*
	 * Close an inputstream
	 */
	private static void close(InputStream is) {
		if( is != null ) {
			try {
				is.close();
			} catch(IOException ie) {
				// IGNORE
			}
		}
	}
	
}
