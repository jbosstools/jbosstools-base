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
	
	public static final String LAUNCHER_ZIP_PREFIX = "launcher/zip";
	
	public static final String LAUNCHER_ZIP_MISSION_PARAMETER_NAME = "mission";
	
	public static final String LAUNCHER_ZIP_RUNTIME_PARAMETER_NAME = "runtime";
	
	public static final String LAUNCHER_ZIP_RUNTIME_VERSION_PARAMETER_NAME = "runtimeVersion";
	
	public static final String LAUNCHER_ZIP_PROJECT_NAME_PARAMETER_NAME = "projectName";
	
	public static final String LAUNCHER_ZIP_GROUP_ID_PARAMETER_NAME = "groupId";
	
	public static final String LAUNCHER_ZIP_ARTIFACT_ID_PARAMETER_NAME = "artifactId";
	
	public static final String LAUNCHER_ZIP_VERSION_PARAMETER_NAME = "projectVersion";

	/**
	 * 
	 */
	private LauncherCoreConstants() {}

}
