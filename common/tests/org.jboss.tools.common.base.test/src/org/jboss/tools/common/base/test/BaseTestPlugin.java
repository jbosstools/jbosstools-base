/*******************************************************************************
 * Copyright (c) 2007-2011 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.base.test;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;

/**
 * The main plugin class to be used in the desktop.
 */
public class BaseTestPlugin extends BaseUIPlugin {
	//The shared instance.
	private static BaseTestPlugin plugin;
	
	public static final String PLUGIN_ID = "org.jboss.tools.common.base.test";  //$NON-NLS-1$

	
	/**
	 * The constructor.
	 */
	public BaseTestPlugin() {
		plugin = this;
	}

	/**
	 * Returns the shared instance.
	 */
	public static BaseTestPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the workspace instance.
	 */
	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}
}
