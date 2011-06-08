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
package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.jboss.tools.runtime.Activator;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.ServerDefinition;
import org.jboss.tools.seam.core.SeamUtil;
import org.jboss.tools.seam.core.project.facet.SeamRuntime;
import org.jboss.tools.seam.core.project.facet.SeamRuntimeManager;
import org.jboss.tools.seam.core.project.facet.SeamVersion;

public class SeamHandler extends AbstractRuntimeDetector implements IJBossRuntimePluginConstants {

	private final static String seamJarName = "jboss-seam.jar";
	private final static String seamVersionAttributeName = "Seam-Version";

	public static File getSeamRoot(ServerDefinition serverDefinition) {
		String type = serverDefinition.getType();
		if (SOA_P.equals(type) || EAP.equals(type) || EPP.equals(type) || EWP.equals(type) ) {
			for (String folder : SEAM_HOME_FOLDER_OPTIONS) {
				File seamFile = new File(serverDefinition.getLocation(),folder); //$NON-NLS-1$
				if (seamFile != null && seamFile.isDirectory()) {
					return seamFile;
				}
			} 
		}
		if (SEAM.equals(type)) {
			return serverDefinition.getLocation();
		}
		return null;
	}
	
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		
		Map<String, SeamRuntime> map = new HashMap<String,SeamRuntime>();

		// to fix https://jira.jboss.org/jira/browse/JBDS-682
		// seam runtime initialization goes throug added servers first and 
		// then process seam runtimes from bundled servers
		for(ServerDefinition serverDefinition:serverDefinitions) {
			if (!serverDefinition.isEnabled()) {
				continue;
			}
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

	public ServerDefinition getServerDefinition(File root,
			IProgressMonitor monitor) {
		if (monitor.isCanceled() || root == null) {
			return null;
		}
		String seamVersion = getSeamVersionFromManifest(root.getAbsolutePath());
		if (seamVersion != null) {
			return new ServerDefinition(root.getName(), seamVersion, SEAM, root.getAbsoluteFile());
		}
		return null;
	}

	public static String getSeamVersionFromManifest(String seamHome) {
		File seamHomeFolder = new File(seamHome);
		if (seamHomeFolder == null || !seamHomeFolder.isDirectory()) {
			return null;
		}
		String[] seamFiles = seamHomeFolder.list(new FilenameFilter() {
			
			public boolean accept(File dir, String name) {
				if ("seam-gen".equals(name)) {
					return true;
				}
				if ("examples".equals(name)) {
					return true;
				}
				return false;
			}
		});
		if (seamFiles == null || seamFiles.length != 2) {
			return null;
		}
		File jarFile = new File(seamHome, "lib/" + seamJarName);
		if(!jarFile.isFile()) {
			jarFile = new File(seamHome, seamJarName);
			if(!jarFile.isFile()) {
				return null;
			}
		}
		try {
			JarFile jar = new JarFile(jarFile);
			Attributes attributes = jar.getManifest().getMainAttributes();
			String version = attributes.getValue(seamVersionAttributeName);
			return version;
		} catch (IOException e) {
			return null;
		}
	}

	public static String included(ServerDefinition serverDefinition) {
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

	@Override
	public boolean exists(ServerDefinition serverDefinition) {
		if (serverDefinition == null || serverDefinition.getLocation() == null) {
			return false;
		}
		File seamRoot = getSeamRoot(serverDefinition);
		if (seamRoot == null || !seamRoot.isDirectory()) {
			return false;
		}
		String path = seamRoot.getAbsolutePath();
		return seamExists(path);
	}
}
