/*************************************************************************************
 * Copyright (c) 2010 JBoss by Red Hat and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.jboss.tools.runtime.Activator;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.JBossRuntimeStartup.IJBossRuntimePersistanceHandler;
import org.jboss.tools.runtime.ServerDefinition;
import org.jboss.tools.seam.core.SeamUtil;
import org.jboss.tools.seam.core.project.facet.SeamRuntime;
import org.jboss.tools.seam.core.project.facet.SeamRuntimeManager;
import org.jboss.tools.seam.core.project.facet.SeamVersion;

public class SeamHandler implements IJBossRuntimePersistanceHandler, IJBossRuntimePluginConstants {
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		
		Map<String, SeamRuntime> map = new HashMap<String,SeamRuntime>();

		// to fix https://jira.jboss.org/jira/browse/JBDS-682
		// seam runtime initialization goes throug added servers first and 
		// then process seam runtimes from bundled servers
		for(ServerDefinition serverDefinition:serverDefinitions) {
			String type = serverDefinition.getType();
			if (SOA_P.equals(type) || EAP.equals(type) || EPP.equals(type) || EWP.equals(type) ) {
				for (String folder : SEAM_HOME_FOLDER_OPTIONS) {
					File seamFile = new File(serverDefinition.getLocation(),folder); //$NON-NLS-1$
					addSeam(map, serverDefinition, seamFile);
				} 
			}
			if (SEAM.equals(type)) {
				addSeam(map, serverDefinition, serverDefinition.getLocation());
			}
		}

		// Initialize Seam Runtime from JBoss EAP
		String seamGenBuildPath = getSeamGenBuildPath(SEAM_1_2_HOME, SEAM_1_2_HOME_CONFIGURATION);
		SeamVersion seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam1(map, seamGenBuildPath, seamVersion);
		
		// fix for https://jira.jboss.org/jira/browse/JBDS-1215 The installer could not find EAP 4.3 Seam runtimes in recent CP
		seamGenBuildPath = getSeamGenBuildPath(SEAM_1_2_HOME_CP, SEAM_1_2_HOME_CONFIGURATION_CP);
		seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam1(map, seamGenBuildPath, seamVersion);

		// Initialize Seam 2.0 Runtime from JBoss EAP
		seamGenBuildPath = getSeamGenBuildPath(SEAM_2_0_HOME, SEAM_2_0_HOME_CONFIGURATION);
		seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam2(map, seamGenBuildPath, seamVersion);
		
		// fix for https://jira.jboss.org/jira/browse/JBDS-1215 The installer could not find EAP 4.3 Seam runtimes in recent CPs
		seamGenBuildPath = getSeamGenBuildPath(SEAM_2_0_HOME_CP, SEAM_2_0_HOME_CONFIGURATION_CP);
		seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam2(map, seamGenBuildPath, seamVersion);
		SeamRuntimeManager.getInstance().save();
	}

	private static void addSeam1(Map<String, SeamRuntime> map,
			String seamGenBuildPath, SeamVersion seamVersion) {
		if (seamVersion != null) {
			StringBuffer name = new StringBuffer("Seam ").append(seamVersion); //$NON-NLS-1$
			if(seamVersion.compareTo(SeamVersion.SEAM_1_2)==0) {
				name.append(".EAP_4.3"); //$NON-NLS-1$
			} else {
				name.append(".EAP5"); //$NON-NLS-1$
			}
			addSeam(map, seamGenBuildPath,seamVersion,name.toString());
		}
	}

	private static void addSeam2(Map<String, SeamRuntime> map,
			String seamGenBuildPath, SeamVersion seamVersion) {
		if (seamVersion != null) {
			String name = "Seam " + seamVersion + ".FP"; //$NON-NLS-1$ //$NON-NLS-2$
			addSeam(map, seamGenBuildPath, seamVersion,name);
		}
	}

	private static void addSeam(Map<String, SeamRuntime> map,
			ServerDefinition serverDefinition, File seamFile) {
		if (seamFile.exists() && seamFile.canRead() && seamFile.isDirectory()) {
			SeamVersion seamVersion = getSeamVersion(seamFile.getAbsolutePath());
			if (seamVersion != null) {
				String name = "Seam " + serverDefinition.getName() + " " + seamVersion; //$NON-NLS-1$ //$NON-NLS-2$
				addSeam(map, seamFile.getAbsolutePath(), seamVersion, name);
			}
		}
	}

	private static void addSeam(Map<String, SeamRuntime> map, String seamPath,SeamVersion seamVersion, String name) {
		if (!seamExists(seamPath)) {
			File seamFolder = new File(seamPath);
			if(seamFolder.exists() && seamFolder.isDirectory()) {
				SeamRuntime rt = new SeamRuntime();
				rt.setHomeDir(seamPath);
				rt.setName(name);
				rt.setDefault(true);
				rt.setVersion(seamVersion);
				SeamRuntimeManager.getInstance().addRuntime(rt);
			}
		}
	}

	private static String getSeamGenBuildPath(String seamHomePath,
			String seamHomePathConfiguration) {
		try {
			Location configLocation = Platform.getConfigurationLocation();
			URL configURL = configLocation.getURL();
			String configuration = FileLocator.resolve(configURL).getPath();
			File seamGenDir = new File(configuration, seamHomePathConfiguration);
			if (!seamGenDir.isDirectory()) {
				String pluginLocation = null;
				pluginLocation = FileLocator
						.resolve(
								Activator.getDefault().getBundle()
										.getEntry("/")).getFile(); //$NON-NLS-1$
				seamGenDir = new File(pluginLocation, seamHomePath);
			}
			Path p = new Path(seamGenDir.getPath());
			p.makeAbsolute();
			if (p.toFile().exists()) {
				return p.toOSString();
			}
		} catch (IOException e) {
			Activator.log(e);
		}
		return ""; //$NON-NLS-1$
	}

	public void importRuntimes() {
		// TODO Auto-generated method stub
		
	}

	public void exportRuntimes() {
		// TODO Auto-generated method stub
		
	}

	public static String includeSeam(ServerDefinition serverDefinition) {
		StringBuilder builder = new StringBuilder();
		for (String folder : IJBossRuntimePluginConstants.SEAM_HOME_FOLDER_OPTIONS) {
			File seamFile = new File(serverDefinition.getLocation(),folder); //$NON-NLS-1$
			if (seamFile.exists() && seamFile.canRead() && seamFile.isDirectory()) {
				SeamVersion seamVersion = getSeamVersion(seamFile.getAbsolutePath());
				if (seamVersion != null) {
					if (builder.toString().length() > 0) {
						builder.append(", ");
					}
					builder.append("Seam ");
					builder.append(seamVersion);
				}
			}
		} 
		return builder.toString();
	}

	public static SeamVersion getSeamVersion(String seamGenBuildPath) {
		if (seamGenBuildPath == null || seamGenBuildPath.trim().length() <= 0) {
			return null;
		}
		String fullVersion = SeamUtil.getSeamVersionFromManifest(seamGenBuildPath);
		if (fullVersion == null) {
			return null;	
		}
		String version = fullVersion.substring(0,3);
		SeamVersion seamVersion = null;
		if (version != null) {
			seamVersion = SeamVersion.findByString(version);
		}
		return seamVersion;
	}
	
	/**
	 * @param seamPath
	 * @return
	 */
	public static boolean seamExists(String seamPath) {
		SeamRuntime[] seamRuntimes = SeamRuntimeManager.getInstance().getRuntimes();
		for (SeamRuntime sr:seamRuntimes) {
			if (seamPath != null && seamPath.equals(sr.getHomeDir())) {
				return true;
			}
		}
		return false;
	}

}
