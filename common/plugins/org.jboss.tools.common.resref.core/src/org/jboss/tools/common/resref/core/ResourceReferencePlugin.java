/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.resref.core;

import org.jboss.tools.foundation.core.plugin.BaseCorePlugin;
import org.jboss.tools.foundation.core.plugin.log.IPluginLog;

/**
 * The activator class controls the plug-in life cycle
 */
public class ResourceReferencePlugin extends BaseCorePlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.common.resref.core"; //$NON-NLS-1$
	public static final String PREFERENCES_QUALIFIER = "org.jboss.tools.common.model"; //$NON-NLS-1$

	// The shared instance
	private static ResourceReferencePlugin plugin;
	
	/**
	 * The constructor
	 */
	public ResourceReferencePlugin() {
		plugin = this;
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static ResourceReferencePlugin getDefault() {
		return plugin;
	}
	
	public static IPluginLog getPluginLog() {
		return plugin.pluginLogInternal();
	}
}