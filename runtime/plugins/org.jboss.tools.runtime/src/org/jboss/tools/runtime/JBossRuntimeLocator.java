/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.ide.eclipse.as.core.server.bean.JBossServerType;
import org.jboss.ide.eclipse.as.core.server.bean.ServerBean;
import org.jboss.ide.eclipse.as.core.server.bean.ServerBeanLoader;

public class JBossRuntimeLocator implements IJBossRuntimePluginConstants {

	public static final String JBPM3 = "jBPM3";
	
	public static final String JBPM4 = "jBPM4";
	private final static String seamJarName = "jboss-seam.jar";
	private final static String seamVersionAttributeName = "Seam-Version";

	public JBossRuntimeLocator() {
	}
	
	public List<ServerDefinition> searchForRuntimes(String path, IProgressMonitor monitor) {
		return searchForRuntimes(new Path(path), monitor);
	}

	public List<ServerDefinition> searchForRuntimes(IPath path, IProgressMonitor monitor) {
		ServerBeanLoader loader = new ServerBeanLoader();
		List<ServerDefinition> serverDefinitions = new ArrayList<ServerDefinition>();
		return searchForRuntimes(serverDefinitions, path, loader, 4, monitor);
	}
	
	private List<ServerDefinition> searchForRuntimes(List<ServerDefinition> serverDefinitions, IPath path, ServerBeanLoader loader, int depth, IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return serverDefinitions;
		}
		File[] children = null;
		if (path != null) {
			monitor.setTaskName("Searching " + path.toOSString());
		}
		if (path != null) {
			File root = path.toFile();
			ServerBean serverBean = loader.loadFromLocation(root);
			
			if (!JBossServerType.UNKNOWN.equals(serverBean.getType())) {
				serverDefinitions.add(new ServerDefinition(serverBean));
				if (monitor.isCanceled()) {
					return serverDefinitions;
				}
			} else {
				String seamVersion = getSeamVersionFromManifest(root.getAbsolutePath());
				if (seamVersion != null) {
					serverDefinitions.add(new ServerDefinition(root.getName(), seamVersion, SEAM, root.getAbsoluteFile()));
					if (monitor.isCanceled()) {
						return serverDefinitions;
					}
				} else {
					String[] files = root.list(new FilenameFilter() {
						
						public boolean accept(File dir, String name) {
							if (name.startsWith("drools-api") && name.endsWith(".jar") ) {
								return true;
							}
							return false;
						}
					});
					boolean droolsFound = false;
					if (files != null && files.length > 0) {
						String version = getImplementationVersion(root,files[0]);
						if (version != null) {
							version = version.substring(0,3);
							serverDefinitions.add(new ServerDefinition(root.getName(), version, DROOLS, root.getAbsoluteFile()));
							droolsFound = true;
						}
					}
					if (monitor.isCanceled()) {
						return serverDefinitions;
					}
					boolean jbpmFound = false;
					if (!droolsFound) {
						boolean isJBPM = isValidJbpmInstallation(root.getAbsolutePath());
						if (isJBPM) {
							String version = "unknown";
							if (isJbpm3(root.getAbsolutePath())) {
								version = JBPM3;
							} else if (isJbpm4(root.getAbsolutePath())) {
								version = JBPM4;
							}
							serverDefinitions.add(new ServerDefinition(root.getName(), version, JBPM, root.getAbsoluteFile()));
							jbpmFound = true;
						}
					}
					if (monitor.isCanceled()) {
						return serverDefinitions;
					}
					if (!droolsFound && !jbpmFound) {
						children = root.listFiles();
					}
				}
				
			}
		} else {
			children = File.listRoots();
		}
		if (monitor.isCanceled()) {
			return serverDefinitions;
		}
		if (depth == 0) {
			return serverDefinitions; 
		}
		if( children != null ) {
			for( int i = 0; i < children.length; i++ ) {
				if( children[i].isDirectory()) {
					searchForRuntimes(serverDefinitions, new Path(children[i].getAbsolutePath()), loader, --depth, monitor);
				}
			}
		}
		return serverDefinitions;
	}

	/**
	 * @param dir 
	 * @param string
	 * @return
	 */
	private String getImplementationVersion(File dir, String file) {
		File jarFile = new File(dir, file);
		if(!jarFile.isFile()) {
			return null;
		}
		try {
			JarFile jar = new JarFile(jarFile);
			Attributes attributes = jar.getManifest().getMainAttributes();
			String version = attributes.getValue("Implementation-Version");
			return version;
		} catch (IOException e) {
			return null;
		}
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
	
	public static boolean isJbpm3(String location) {
		return new Path(location).append("/src/resources/gpd/version.info.xml").toFile().exists();
	}
	
	public static boolean isJbpm4(String location) {
		return new Path(location).append("/jbpm.jar").toFile().exists();
	}
	
	private boolean isValidJbpmInstallation(String location) {
		return isJbpm3(location) || isJbpm4(location);
	}
}
