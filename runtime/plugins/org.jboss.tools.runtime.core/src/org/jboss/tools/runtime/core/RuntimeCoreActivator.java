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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.InvalidRuntimeDetector;
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
	private static final String RUNTIME_DETECTOR_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeDetectors";
	
	// Extension point property keys
	private static final String NAME = "name";
	private static final String PREFERENCE_ID = "preferenceId";
	private static final String ID = "id";
	private static final String ENABLED = "enabled";
	private static final String PRIORITY = "priority";

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

	public static IRuntimeDetector getEsbDetector() {
		return getOrLoadDefaultInstance().getEsbDetector2();
	}

	private static RuntimeCoreActivator getOrLoadDefaultInstance() {
		if( getDefault() == null ) {
			// load bundle
			Bundle bundle = Platform.getBundle(PLUGIN_ID);
			try {
				bundle.start();
			} catch(BundleException be) {
				be.printStackTrace();
			}
		}
		return getDefault();
	}
	// Convenience method
	public static Set<IRuntimeDetector> getDeclaredRuntimeDetectors() {
		return getOrLoadDefaultInstance().getDeclaredRuntimeDetectors2();
	}
	
	// Load all declared runtime detectors
	public Set<IRuntimeDetector> getDeclaredRuntimeDetectors2() {
		if( declaredRuntimeDetectors == null) {
			declaredRuntimeDetectors = loadDeclaredRuntimeDetectors();
		}
		return declaredRuntimeDetectors;
	}
	
	
	// This method will do a full load and actually instantiate the classes
	public Set<IRuntimeDetector> loadDeclaredRuntimeDetectors() {
		Set<IRuntimeDetector> declared = new TreeSet<IRuntimeDetector>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry
				.getExtensionPoint(RUNTIME_DETECTOR_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IRuntimeDetector dec = loadOneDeclaredRuntimeDetector(configurationElements[j]); 
				if( !declared.contains(dec)) {
					declared.add(dec);
				}
			}
		}
		return declared;
	}

	// This method will load one detector from a configuration element
	private IRuntimeDetector loadOneDeclaredRuntimeDetector(IConfigurationElement configurationElement) {
		IRuntimeDetector detector;
		try {
			detector = (IRuntimeDetector) configurationElement.createExecutableExtension("class");
		} catch (CoreException e) {
			log(e);
			detector = new InvalidRuntimeDetector();
			detector.setValid(false);
		}
		String name = configurationElement.getAttribute(NAME);
		String preferenceId = configurationElement.getAttribute(PREFERENCE_ID);
		String id = configurationElement.getAttribute(ID);
		detector.setName(name);
		detector.setPreferenceId(preferenceId);
		detector.setId(id);
		String enabled = configurationElement.getAttribute(ENABLED);
		if (enabled == null || new Boolean(enabled).booleanValue()) {
			detector.setEnabled(true);
		} else {
			detector.setEnabled(false);
		}
		String priorityString = configurationElement
				.getAttribute(PRIORITY);
		int priority;
		try {
			priority = Integer.parseInt(priorityString);
		} catch (Exception ex) {
			priority = Integer.MAX_VALUE;
		}
		detector.setPriority(priority);
		return detector;
	}
	
	public static Set<IRuntimeDetector> getRuntimeDetectors() {
		return getOrLoadDefaultInstance().getRuntimeDetectors2();
	}
	
	public Set<IRuntimeDetector> getRuntimeDetectors2() {
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

	public static void saveEnabledDetectors(Set<IRuntimeDetector> allDetectors) {
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
	
	private static IEclipsePreferences getPreferences() {
		return getOrLoadDefaultInstance().getPreferences2();
	}
	
	private IEclipsePreferences getPreferences2() {
		if (prefs == null) {
			prefs = ConfigurationScope.INSTANCE.getNode(PLUGIN_ID);
		}
		return prefs;
	}
	
	public IRuntimeDetector getEsbDetector2() {
		if (esbDetector == null) {
			for (IRuntimeDetector detector:getDeclaredRuntimeDetectors()) {
				if (ESB_DETECTOR_ID.equals(detector.getId())) {
					esbDetector = detector;
				}
			}
		}
		return esbDetector;
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
