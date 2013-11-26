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
import org.jboss.tools.common.log.BasePlugin;
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
public class RuntimeCoreActivator extends BasePlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.runtime.core"; //$NON-NLS-1$

	// The shared instance
	private static RuntimeCoreActivator plugin;

	// Member variables
	private Set<IRuntimeDetector> runtimeDetectors;
	
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
		for (IRuntimeDetector detector:getRuntimeDetectors()) {
			if (id.equals(detector.getId())) {
				return detector;
			}
		}
		return null;
	}

	/**
	 * Please use getRuntimeDetectors 
	 * @see RuntimeCoreActivator#getRuntimeDetectors()
	 */
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
		if (runtimeDetectors == null) {
			runtimeDetectors = RuntimeExtensionManager.getDefault().loadInitializedRuntimeDetectors();
		}
		return runtimeDetectors;
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
	 * This method may be long-running depending on the state of the cache.
	 * It may load plugins or access remote resources.
	 * Please use the signature with the progress monitor instead
	 * 
	 * @deprecated
	 * @See {@link RuntimeCoreActivator#getDownloadRuntimes(IProgressMonitor)}
	 * @return
	 */
	public Map<String, DownloadRuntime> getDownloadRuntimes() {
		return RuntimeExtensionManager.getDefault().getDownloadRuntimes();
	}
	
	
	/**
	 * Get a map of download runtime ID to the actual downloadruntime object
	 * Warning:  This method may involve plugin loading or long-running file or wire IO tasks.
	 * This should not be called from the UI without the ability to respond to progress or initiate a cancelation
	 * @return
	 */
	public Map<String, DownloadRuntime> getDownloadRuntimes(IProgressMonitor monitor) {
		return RuntimeExtensionManager.getDefault().getDownloadRuntimes(monitor);
	}

	/**
	 * Get an array of download runtime objects
	 * 
	 * Warning:  This method may involve plugin loading or long-running file or wire IO tasks.
	 * This should not be called from the UI without the ability to respond to progress or initiate a cancelation
	 * 
	 * @return
	 */
	public DownloadRuntime[] getDownloadRuntimeArray(IProgressMonitor monitor) {
		Map<String, DownloadRuntime> map = RuntimeExtensionManager.getDefault().getDownloadRuntimes(monitor);
		if( map == null )
			return new DownloadRuntime[0];
		Collection<DownloadRuntime> arr = map.values();
		return (DownloadRuntime[]) arr.toArray(new DownloadRuntime[arr.size()]);
	}

	
	/**
	 * This method will check for a download runtime by checking it's
	 * id, or, if none is found, by checking for a PROPERTY_ALTERNATE_ID
	 * property key which matches the id. 
	 * 
	 * This method may be long-running depending on the state of the cache.
	 * It may load plugins or access remote resources.
	 * Please use the signature with the progress monitor instead
	 * 
	 * @deprecated
	 * @see RuntimeCoreActivator#findDownloadRuntime(String, IProgressMonitor)
	 * 
	 * @param id A found DownloadRuntime or null
	 * @return
	 * 
	 */
	public DownloadRuntime findDownloadRuntime(String id) {
		return RuntimeExtensionManager.getDefault().findDownloadRuntime(id);
	}
	
	/**
	 * This method will check for a download runtime by checking it's
	 * id, or, if none is found, by checking for a PROPERTY_ALTERNATE_ID
	 * property key which matches the id. 
	 * 
	 * @param id A found DownloadRuntime or null
	 * @param IProgressMonitor monitor
	 * @return
	 */
	public DownloadRuntime findDownloadRuntime(String id, IProgressMonitor monitor) {
		return RuntimeExtensionManager.getDefault().findDownloadRuntime(id, monitor);
	}
}
