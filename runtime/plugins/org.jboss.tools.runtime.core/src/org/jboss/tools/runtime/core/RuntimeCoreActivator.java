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

import java.util.Map;
import java.util.Set;

import org.jboss.tools.common.log.BasePlugin;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
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
	private static final String ESB_DETECTOR_ID = "org.jboss.tools.runtime.handlers.EsbHandler"; //$NON-NLS-1$

	// The shared instance
	private static RuntimeCoreActivator plugin;

	// Member variables
	private Set<IRuntimeDetector> declaredRuntimeDetectors;
	private Set<IRuntimeDetector> runtimeDetectors;
	private IRuntimeDetector esbDetector;
	private Map<String, DownloadRuntime> downloadRuntimes;
	
	private BundleContext context;
	
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
	
	public IRuntimeDetector getEsbDetector() {
		if (esbDetector == null) {
			for (IRuntimeDetector detector:getDeclaredRuntimeDetectors()) {
				if (ESB_DETECTOR_ID.equals(detector.getId())) {
					esbDetector = detector;
				}
			}
		}
		return esbDetector;
	}

	/*
	 * Is this really necessary??
	 * 
	 * getDeclaredRuntimeDetectors and getRuntimeDetectors
	 * does almost exactly the same thing, except getRuntimeDetectors
	 * sets the enablement and getDeclared... does not. 
	 * 
	 * Why would anyone require uninitialized detectors?
	 * Couldn't they simply ignore the enabled flag? 
	 */
	public Set<IRuntimeDetector> getDeclaredRuntimeDetectors() {
		if( declaredRuntimeDetectors == null) {
			declaredRuntimeDetectors = RuntimeExtensionManager.getDefault().loadDeclaredRuntimeDetectors();
		}
		return declaredRuntimeDetectors;
	}
	
	public Set<IRuntimeDetector> getRuntimeDetectors() {
		if (runtimeDetectors == null) {
			runtimeDetectors = RuntimeExtensionManager.getDefault().loadInitializedRuntimeDetectors();
		}
		return runtimeDetectors;
	}

	public void saveEnabledDetectors() {
		saveEnabledDetectors(getDeclaredRuntimeDetectors());
	}
	
	public void saveEnabledDetectors(Set<IRuntimeDetector> allDetectors) {
		RuntimeCorePreferences.getDefault().saveEnabledDetectors(allDetectors);
	}
	
	public Map<String, DownloadRuntime> getDownloadRuntimes() {
		if( downloadRuntimes == null )
			downloadRuntimes = RuntimeExtensionManager.getDefault().loadDownloadRuntimes();
		return downloadRuntimes;
	}
}
