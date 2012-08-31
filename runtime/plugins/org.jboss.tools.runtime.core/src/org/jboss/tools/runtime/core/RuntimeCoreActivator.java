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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.service.prefs.BackingStoreException;

/**
 * The activator class controls the plug-in life cycle
 * 
 * @author snjeza
 */
public class RuntimeCoreActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.runtime.core"; //$NON-NLS-1$
	private static final String ESB_DETECTOR_ID = "org.jboss.tools.runtime.handlers.EsbHandler"; //$NON-NLS-1$

	// Preference key
	private static final String ENABLED_DETECTORS = "enabledDetectors";

	// The shared instance
	private static RuntimeCoreActivator plugin;

	// Member variables
	private Set<IRuntimeDetector> declaredRuntimeDetectors;
	private Set<IRuntimeDetector> runtimeDetectors;
	private IRuntimeDetector esbDetector;
	
	private IEclipsePreferences prefs;
	
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

	// Convenience method
	public Set<IRuntimeDetector> getDeclaredRuntimeDetectors() {
		if( declaredRuntimeDetectors == null) {
			declaredRuntimeDetectors = RuntimeExtensionManager.getDefault().loadDeclaredRuntimeDetectors();
		}
		return declaredRuntimeDetectors;
	}
	
	public Set<IRuntimeDetector> getRuntimeDetectors() {
		if (runtimeDetectors == null) {
			Set<IRuntimeDetector> tmp = getDeclaredRuntimeDetectors();
			String enabledDetectors = getPreferences().get(ENABLED_DETECTORS,
					null);
			if (enabledDetectors == null) {
				saveEnabledDetectors(tmp);
			} else {
				StringTokenizer tokenizer = new StringTokenizer(
						enabledDetectors, ",");
				List<String> enabled = new ArrayList<String>();
				while (tokenizer.hasMoreTokens()) {
					String token = tokenizer.nextToken();
					if (token != null && !token.isEmpty()) {
						enabled.add(token);
					}
				}
				for (IRuntimeDetector detector : tmp) {
					detector.setEnabled(enabled.contains(detector.getId()));
				}
			}
			runtimeDetectors = tmp;
		}
		return runtimeDetectors;
	}

	public void saveEnabledDetectors() {
		saveEnabledDetectors(getDeclaredRuntimeDetectors());
	}
	
	public void saveEnabledDetectors(Set<IRuntimeDetector> allDetectors) {
		StringBuilder builder = new StringBuilder();
		for (IRuntimeDetector detector:allDetectors) {
			if (detector.isEnabled()) {
				builder.append(detector.getId());
				builder.append(",");
			}
		}
		String enabled = builder.toString();
		int index = enabled.lastIndexOf(",");
		if (index != -1) {
			enabled = enabled.substring(0, index);
		}
		getPreferences().put(ENABLED_DETECTORS, enabled);
		try {
			getPreferences().flush();
		} catch (BackingStoreException e) {
			log(e);
		}
	}
	
	private  IEclipsePreferences getPreferences() {
		if (prefs == null) {
			prefs = ConfigurationScope.INSTANCE.getNode(PLUGIN_ID);
		}
		return prefs;
	}
	
	public static void log(Throwable e) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, e
				.getLocalizedMessage(), e);
		getDefault().getLog().log(status);
	}
	
	public static void log(Throwable e, String message) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, message, e);
		getDefault().getLog().log(status);
	}
	
}
