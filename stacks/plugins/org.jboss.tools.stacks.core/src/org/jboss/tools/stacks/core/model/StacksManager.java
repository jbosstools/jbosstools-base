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
import org.jboss.jdf.stacks.client.StacksClient;
import org.jboss.jdf.stacks.client.StacksClientConfiguration;
import org.jboss.jdf.stacks.model.Stacks;
import org.jboss.jdf.stacks.parser.Parser;
import org.jboss.tools.foundation.ecf.URLTransportUtility;
import org.jboss.tools.stacks.core.StacksCoreActivator;

public class StacksManager {

	/**
	 * TODO This property string could be changed?
	 */
	private static final String STACKS_URL_PROPERTY = "org.jboss.examples.stacks.url";
	private static final String STACKS_URL;
	
	// TODO move into client jar also? 
	private static final String PRESTACKS_URL = "https://raw.github.com/jboss-jdf/jdf-stack/1.0.0.Final/pre-stacks.yaml";
	
	// Declare the types of stacks available for fetch
	public enum StacksType {
		STACKS_TYPE, PRESTACKS_TYPE
	}
	
	// Load the default stacks url from a sysprop or jar
	static {
		String defaultUrl = getStacksUrlFromJar(); //$NON-NLS-1$
		STACKS_URL = System.getProperty(
				STACKS_URL_PROPERTY, defaultUrl); //$NON-NLS-1$
	}
	
	public Stacks getStacks(IProgressMonitor monitor) {
		return getStacks(STACKS_URL, "stacks.yaml", monitor);
	}
	
	public Stacks[] getStacks(String jobName, IProgressMonitor monitor, StacksType... types) {
		ArrayList<Stacks> ret = new ArrayList<Stacks>();
		monitor.beginTask(jobName, types.length * 100);
		for( int i = 0; i < types.length; i++ ) {
			String url = null;
			switch(types[i] ) {
			case STACKS_TYPE:
				url = STACKS_URL;
				break;
			case PRESTACKS_TYPE:
				url = PRESTACKS_URL;
				break;
			default:
				break;
			}
			if( url != null ) {
				ret.add(getStacks(url, jobName, new SubProgressMonitor(monitor, 100)));
			}
		}
		monitor.done();
		return (Stacks[]) ret.toArray(new Stacks[ret.size()]);
	}
	
	// Added for easier testing  Please use other signatures. 
	@Deprecated
	protected Stacks getStacks(String url, String prefix, String suffix, IProgressMonitor monitor) {
		String jobName = prefix + "." + suffix;
		return getStacks(url, jobName, monitor);
	}
	
	protected Stacks getStacks(String url, String jobName, IProgressMonitor monitor) {
		Stacks stacks = null;
		try {
			File f = getCachedFileForURL(url, jobName, monitor);
			if (f != null && f.exists()) {
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
		if (stacks == null) {
			StacksCoreActivator.pluginLog().logWarning("Stacks from "+ url +" can not be read, falling back on default Stacks Client values");
			StacksClient client = new StacksClient();
			stacks = client.getStacks();
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
		 return new URLTransportUtility().getCachedFileForURL(url, jobName, URLTransportUtility.CACHE_FOREVER, monitor);
	}
	

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
