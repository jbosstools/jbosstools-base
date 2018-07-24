/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.core;

/**
 * Constants
 */
public final class LauncherCoreConstants {
	
	/**
	 * Name of the Launcher endpoint URL preference 
	 */
	public static final String LAUNCHER_ENDPOINT_PREFERENCE_NAME = LauncherCorePlugin.PLUGIN_ID + ".endpoint";
	public static final String LAUNCHER_ENDPOINT_PREFERENCE_DEFAULT = "https://forge.api.openshift.io/api";
	
	
	public static final String LAUNCHER_CATALOG_SUFFIX = "booster-catalog";

	/**
	 * 
	 */
	private LauncherCoreConstants() {}

}
