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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.seam.core.SeamUtil;
import org.jboss.tools.seam.core.project.facet.SeamRuntime;
import org.jboss.tools.seam.core.project.facet.SeamRuntimeManager;
import org.jboss.tools.seam.core.project.facet.SeamVersion;

public class SeamHandler extends AbstractRuntimeDetector {

	private final static String seamJarName = "jboss-seam.jar";
	private final static String seamVersionAttributeName = "Seam-Version";
	private static final String SEAM = "SEAM"; // NON-NLS-1$
	
	private static File getSeamRoot(RuntimeDefinition serverDefinition) {
		String type = serverDefinition.getType();
		if (SEAM.equals(type)) {
			return serverDefinition.getLocation();
		}
		return null;
	}
	
	@Override
	public void initializeRuntimes(List<RuntimeDefinition> serverDefinitions) {
		
		Map<String, SeamRuntime> map = new HashMap<String,SeamRuntime>();

		for(RuntimeDefinition serverDefinition:serverDefinitions) {
			if (serverDefinition.isEnabled()) {
				String type = serverDefinition.getType();
				if (SEAM.equals(type)) {
					addSeam(map, serverDefinition,
							serverDefinition.getLocation());
				}
			}
			initializeRuntimes(serverDefinition.getIncludedServerDefinitions());
		}
		SeamRuntimeManager.getInstance().save();
	}

	private static void addSeam(Map<String, SeamRuntime> map,
			RuntimeDefinition serverDefinition, File seamFile) {
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

	private static SeamVersion getSeamVersion(String seamGenBuildPath) {
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
	private static boolean seamExists(String seamPath) {
		SeamRuntime[] seamRuntimes = SeamRuntimeManager.getInstance().getRuntimes();
		for (SeamRuntime sr:seamRuntimes) {
			if (seamPath != null && seamPath.equals(sr.getHomeDir())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public RuntimeDefinition getServerDefinition(File root,
			IProgressMonitor monitor) {
		if (monitor.isCanceled() || root == null) {
			return null;
		}
		String seamVersion = getSeamVersionFromManifest(root.getAbsolutePath());
		if (seamVersion != null) {
			return new RuntimeDefinition(root.getName(), seamVersion, SEAM, root.getAbsoluteFile());
		}
		return null;
	}

	private static String getSeamVersionFromManifest(String seamHome) {
		File seamHomeFolder = new File(seamHome);
		if (seamHomeFolder == null || !seamHomeFolder.isDirectory()) {
			return null;
		}
		String[] seamFiles = seamHomeFolder.list(new FilenameFilter() {
			
			public boolean accept(File dir, String name) {
				if ("seam-gen".equals(name)) {
					return true;
				}
				if ("lib".equals(name)) {
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
			if (version == null) {
				// Seam 2.3
				version = attributes.getValue("Implementation-Version");
			}
			return version;
		} catch (IOException e) {
			return null;
		}
	}

	@Override
	public boolean exists(RuntimeDefinition serverDefinition) {
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
