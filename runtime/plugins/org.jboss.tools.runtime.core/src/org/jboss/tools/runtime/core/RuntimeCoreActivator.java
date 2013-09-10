/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.foundation.core.plugin.BaseCorePlugin;
import org.jboss.tools.foundation.core.plugin.log.IPluginLog;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;
import org.jboss.tools.runtime.core.internal.RuntimeCorePreferences;
import org.jboss.tools.runtime.core.internal.RuntimeExtensionManager;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimes;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 * 
 * @author snjeza
 */
public class RuntimeCoreActivator extends BaseCorePlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.runtime.core"; //$NON-NLS-1$

	// The shared instance
	private static RuntimeCoreActivator plugin;

	private BundleContext context;
	private IDownloadRuntimes downloader = null;
	
	/**
	 * The constructor
	 */
	public RuntimeCoreActivator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		this.context = context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static RuntimeCoreActivator getDefault() {
		return plugin;
	}

	public BundleContext getBundleContext() {
		return context;
	}
	
	public IDownloadRuntimes getDownloader() {
		return downloader;
	}
	
	public void setDownloader(IDownloadRuntimes downloader) {
		this.downloader = downloader;
	}
	
	// TODO figure out a better place for this
	private static final String ESB_DETECTOR_ID = "org.jboss.tools.runtime.handlers.EsbHandler"; //$NON-NLS-1$
	public IRuntimeDetector getEsbDetector() {
		return findRuntimeDetector(ESB_DETECTOR_ID);
	}
	
	public IRuntimeDetector findRuntimeDetector(String id) {
		return RuntimeExtensionManager.getDefault().findRuntimeDetector(id);
	}

	/* Please use getRuntimeDetectors */
	@Deprecated
	public Set<IRuntimeDetector> getDeclaredRuntimeDetectors() {
		return getRuntimeDetectors();
	}
	
	/**
	 * This method is dangerous to use. Do NOT modify the returning Set, 
	 * as it is in fact the actual data model! 
	 * @return
	 */
	public synchronized Set<IRuntimeDetector> getRuntimeDetectors() {
		return RuntimeExtensionManager.getDefault().getRuntimeDetectors();
	}

	public void saveEnabledDetectors() {
		saveEnabledDetectors(getRuntimeDetectors());
	}
	
	public void saveEnabledDetectors(Set<IRuntimeDetector> allDetectors) {
		RuntimeCorePreferences.getDefault().saveEnabledDetectors(allDetectors);
	}
	
	/**
	 * Get a map of download runtime ID to the actual downloadruntime object
	 * This task may be long-running. It is advised to use the 
	 * signature with the progress monitor instead. 
	 * 
	 * @return
	 */
	public Map<String, DownloadRuntime> getDownloadRuntimes() {
		return RuntimeExtensionManager.getDefault().getDownloadRuntimes();
	}
	
	
	/**
	 * Get a map of download runtime ID to the actual downloadruntime object
	 * @return
	 */
	public Map<String, DownloadRuntime> getDownloadRuntimes(IProgressMonitor monitor) {
		return RuntimeExtensionManager.getDefault().getDownloadRuntimes(monitor);
	}

	/**
	 * Get an array of download runtime objects
	 * @return
	 */
	public DownloadRuntime[] getDownloadRuntimeArray(IProgressMonitor monitor) {
		Map<String, DownloadRuntime> map = RuntimeExtensionManager.getDefault().getDownloadRuntimes(monitor);
		Collection<DownloadRuntime> arr = map.values();
		return (DownloadRuntime[]) arr.toArray(new DownloadRuntime[arr.size()]);
	}

	
	/**
	 * This method will check for a download runtime by checking it's
	 * id, or, if none is found, by checking for a PROPERTY_ALTERNATE_ID
	 * property key which matches the id. 
	 * 
	 * @param id A found DownloadRuntime or null
	 * @return
	 */
	public DownloadRuntime findDownloadRuntime(String id) {
		return RuntimeExtensionManager.getDefault().findDownloadRuntime(id);
	}
	
	/**
	 * Get the IPluginLog for this plugin. This method 
	 * helps to make logging easier, for example:
	 * 
	 *     FoundationCorePlugin.pluginLog().logError(etc)
	 *  
	 * @return IPluginLog object
	 */
	public static IPluginLog pluginLog() {
		return getDefault().pluginLogInternal();
	}

	/**
	 * Get a status factory for this plugin
	 * @return status factory
	 */
	public static StatusFactory statusFactory() {
		return getDefault().statusFactoryInternal();
	}
}
