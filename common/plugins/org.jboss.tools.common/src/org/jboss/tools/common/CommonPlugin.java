/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Dictionary;
import java.util.Properties;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.log.BasePlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.osgi.framework.Bundle;

public class CommonPlugin extends BasePlugin {

	public static final String PLUGIN_ID = "org.jboss.tools.common";
	protected static CommonPlugin instance;
	private static String environment;

	
	public CommonPlugin() {
		super();
		instance = this;
	}

	public static CommonPlugin getInstance() {
	    return instance;
	}

	/**
	 * Gets message from plugin.properties
	 * @param key
	 * @return
	 */
	public static String getMessage(String key)	{
		return Platform.getResourceString(instance.getBundle(), key);
	}

	/**
	 * @return Studio enveroment.
	 */
	public static String getEnvironment() {
		if(environment == null) {
			String osName = System.getProperty("os.name");
			String javaVersion = System.getProperty("java.version");
			String studioName = "unknown";
			String studioVersion = "unknown";
			String eclipseVersion = "unknown";
			String eclipseBuildId = "unknown";

			Bundle studio = Platform.getBundle("org.jboss.tools.common");
			if(studio!=null) {
				Dictionary studioDic = studio.getHeaders();
				studioName = (String)studioDic.get("Bundle-Name");
				studioVersion = (String)studioDic.get("Bundle-Version");
			}

			Bundle eclipse = Platform.getBundle("org.eclipse.platform");
			if(eclipse!=null) {
				Dictionary eclipseDic = eclipse.getHeaders();
				eclipseVersion = (String)eclipseDic.get("Bundle-Version");
				try {
					String path = FileLocator.resolve(eclipse.getEntry("/")).getPath();
					if(path!=null) {
						File aboutMappings = new File(path, "about.mappings");
						if(aboutMappings.exists()) {
							Properties properties = new Properties();
							properties.load(new FileInputStream(aboutMappings));
							String buildId = properties.getProperty("0");
							if(buildId!=null && buildId.length()>0) {
								eclipseBuildId = buildId;
							}
						}
					}
				} catch (IOException e) {
					getPluginLog().logError("Error in getting environment info: " + e.getMessage());
				}
			}
			StringBuffer result = new StringBuffer(studioName).append(" ").append(studioVersion).
				append(", Eclipse ").append(eclipseVersion).append(" ").
				append(eclipseBuildId).append(", Java ").append(javaVersion).
				append(", ").append(osName);
			environment = result.toString();
		}
		return environment;
	}

	
	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static CommonPlugin getDefault() {
		return instance;
	}

	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}
}
