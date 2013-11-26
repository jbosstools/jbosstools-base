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
import org.jboss.jdf.stacks.client.DefaultStacksClientConfiguration;
import org.jboss.jdf.stacks.client.StacksClient;
import org.jboss.jdf.stacks.client.StacksClientConfiguration;
import org.jboss.jdf.stacks.client.messages.StacksMessages;
import org.jboss.jdf.stacks.model.Stacks;
import org.jboss.jdf.stacks.parser.Parser;
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
	
	// TODO move into client jar also? 
	private static final String PRESTACKS_DEFAULT_URL = "https://raw.github.com/jboss-jdf/jdf-stack/1.0.0.Final/pre-stacks.yaml";
	
	// Declare the types of stacks available for fetch
	public enum StacksType {
		STACKS_TYPE, PRESTACKS_TYPE
	}
	
	// Load the default stacks url and prestacks url from a sysprop or jar
	static {
		String defaultUrl = getStacksUrlFromJar(); //$NON-NLS-1$
		STACKS_URL = System.getProperty(URL_PROPERTY_STACKS, System.getProperty(STACKS_URL_PROPERTY, System.getProperty(StacksClientConfiguration.REPO_PROPERTY, defaultUrl)));
		// should use StacksCC prestacks property but that requires update of library. can be done after GA
		PRESTACKS_URL = System.getProperty(URL_PROPERTY_PRESTACKS, System.getProperty("jdf.prestacks.client.repo", PRESTACKS_DEFAULT_URL)); //$NON-NLS-1$ 
	}
	
	/**
	 * Fetch the default stacks model. 
	 * 
	 * @param monitor
	 * @return
	 */
	public Stacks getStacks(IProgressMonitor monitor) {
		return getStacks(STACKS_URL, "stacks.yaml", monitor);
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
				ret.add(getStacks(STACKS_URL, jobName, new SubProgressMonitor(monitor, 100)));
				break;
			case PRESTACKS_TYPE:
				Trace.trace(Trace.STRING_FINEST, "Loading Pre-Stacks Model from " + PRESTACKS_URL);
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
		Stacks stacks = null;
		try {
			Trace.trace(Trace.STRING_FINEST, "Locating or downloading file for " + url);
			File f = getCachedFileForURL(url, jobName, cacheType, monitor);
			if (f != null && f.exists()) {
				Trace.trace(Trace.STRING_FINEST, "Local file for url exists");
				FileInputStream fis = null;
				try {
					fis = new FileInputStream(f);
					Parser p = new Parser();
					stacks = p.parse(fis);
				} finally {
					close(fis);
				}
			}
		} catch (Exception e) {
			StacksCoreActivator.pluginLog().logError("Can't access or parse  " + url, e ); //$NON-NLS-1$
		}
		
		// Check using the stacks client, but only if the URL in the client is different
		if (stacks == null && !monitor.isCanceled() ) {
			// if jar url is not the same as the one we just checked, set to online mode
			final boolean useOnline = !url.equals(getStacksUrlFromJar());
			StacksCoreActivator.pluginLog().logWarning("Stacks from "+ url +" can not be read, falling back on default Stacks Client values");
			DefaultStacksClientConfiguration config = new DefaultStacksClientConfiguration();
			config.setOnline(useOnline);
			final StacksClient client = new StacksClient(config, new JBTStacksMessages());
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
			stacks = (Stacks)ret;
		}
		return stacks;
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
			is = StacksManager.class.getResourceAsStream("/org/jboss/jdf/stacks/client/config.properties"); //$NON-NLS-1$
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
