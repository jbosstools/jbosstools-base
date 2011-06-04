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
import org.osgi.framework.BundleContext;
import org.osgi.service.prefs.BackingStoreException;

/**
 * The activator class controls the plug-in life cycle
 * 
 * @author snjeza
 */
public class RuntimeCoreActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.runtime.core"; //$NON-NLS-1$

	private static final String RUNTIME_DETECTOR_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeDetectors";

	private static final String NAME = "name";

	private static final String PREFERENCE_ID = "preferenceId";
	
	private static final String ID = "id";
	
	private static final String ENABLED = "enabled";

	private static final String ENABLED_DETECTORS = "enabledDetectors";

	private static final String PRIORITY = "priority";

	private static Set<IRuntimeDetector> declaredRuntimeDetectors;
	
	private static Set<IRuntimeDetector> runtimeDetectors;
	
	// The shared instance
	private static RuntimeCoreActivator plugin;

	private static IEclipsePreferences prefs;
	
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

	public static Set<IRuntimeDetector> getDeclaredRuntimeDetectors() {
		if (declaredRuntimeDetectors == null) {
			declaredRuntimeDetectors = new TreeSet<IRuntimeDetector>();
			IExtensionRegistry registry = Platform.getExtensionRegistry();
			IExtensionPoint extensionPoint = registry
					.getExtensionPoint(RUNTIME_DETECTOR_EXTENSION_ID);
			IExtension[] extensions = extensionPoint.getExtensions();
			for (int i = 0; i < extensions.length; i++) {
				IExtension extension = extensions[i];
				IConfigurationElement[] configurationElements = extension
						.getConfigurationElements();
				for (int j = 0; j < configurationElements.length; j++) {
					IConfigurationElement configurationElement = configurationElements[j];
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
					if (enabled == null || "true".equals(enabled)) {
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
					declaredRuntimeDetectors.add(detector);
				}
			}	
		}
		return declaredRuntimeDetectors;
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

	public static Set<IRuntimeDetector> getRuntimeDetectors() {
		if (runtimeDetectors == null) {
			runtimeDetectors = getDeclaredRuntimeDetectors();
			String enabledDetectors = getPreferences().get(ENABLED_DETECTORS,
					null);
			if (enabledDetectors == null) {
				saveEnabledDetectors(runtimeDetectors);
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
				for (IRuntimeDetector detector : runtimeDetectors) {
					detector.setEnabled(enabled.contains(detector.getId()));
				}
			}
		}
		return runtimeDetectors;
	}

	public static void saveEnabledDetectors(Set<IRuntimeDetector> detectors) {
		StringBuilder builder = new StringBuilder();
		for (IRuntimeDetector detector:detectors) {
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
		if (prefs == null) {
			prefs = new ConfigurationScope().getNode(PLUGIN_ID);
		}
		return prefs;
	}
	
}
