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
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.jbpm.preferences.JbpmInstallation;
import org.jboss.tools.jbpm.preferences.PreferencesManager;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.ServerDefinition;

public class JbpmHandler extends AbstractRuntimeDetector implements IJBossRuntimePluginConstants {
	
	private static final String JBPM3 = "jBPM3";
	
	private static final String JBPM4 = "jBPM4";
	
	public static File getJbpmRoot(ServerDefinition serverDefinition) {
		String type = serverDefinition.getType();
		if (SOA_P.equals(type) || SOA_P_STD.equals(type)) {
			return new File(serverDefinition.getLocation(),"jbpm-jpdl"); //$NON-NLS-1$
		}
		if (JBPM.equals(type)) {
			return serverDefinition.getLocation();
		}
		return null;
	}
	
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		for (ServerDefinition serverDefinition : serverDefinitions) {
			if (serverDefinition.isEnabled() && !jbpmExists(serverDefinition)) {
				File jbpmRoot = getJbpmRoot(serverDefinition);
				if (jbpmRoot == null || !jbpmRoot.isDirectory()) {
					continue;
				}
				String type = serverDefinition.getType();
				if (SOA_P.equals(type) || SOA_P_STD.equals(type)) {
					if (jbpmRoot.isDirectory()) {
						String version = JBPM3;
						if (isJbpm4(serverDefinition.getLocation().getAbsolutePath())) {
							version = JBPM4;
						}
						PreferencesManager.getInstance().initializeDefaultJbpmInstallation(serverDefinition.getName(), jbpmRoot.getAbsolutePath(), version);
					}
				} else if (JBPM.equals(type)) {
					PreferencesManager.getInstance().addJbpmInstallation(serverDefinition.getName(), jbpmRoot.getAbsolutePath(), serverDefinition.getVersion());
				}
			}
		}
		
	}
	
	/**
	 * @param serverDefinition
	 * @return
	 */
	public static boolean jbpmExists(ServerDefinition serverDefinition) {
		File jbpmRoot = getJbpmRoot(serverDefinition);
		if (jbpmRoot == null || !jbpmRoot.isDirectory()) {
			return false;
		}
		Map<String, JbpmInstallation> jbpmMap = PreferencesManager.getInstance().getJbpmInstallationMap();
		Collection<JbpmInstallation> jbpmInstalations = jbpmMap.values();
		for (JbpmInstallation jbpm:jbpmInstalations) {
			String location = jbpm.location;
			if (location != null && location.equals(jbpmRoot.getAbsolutePath())) {
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
		boolean isJBPM = isValidJbpmInstallation(root.getAbsolutePath());
		if (isJBPM) {
			String version = "unknown";
			if (isJbpm3(root.getAbsolutePath())) {
				version = JBPM3;
			} else if (isJbpm4(root.getAbsolutePath())) {
				version = JBPM4;
			}
			return new ServerDefinition(root.getName(), version, JBPM, root.getAbsoluteFile());
		}
		return null;
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
	
	public static String included(ServerDefinition serverDefinition) {
		StringBuilder builder = new StringBuilder();
		File jbpmRoot = new File(serverDefinition.getLocation(),"jbpm-jpdl"); //$NON-NLS-1$
		if (jbpmRoot.isDirectory()) {
			String version = JBPM3;
			if (isJbpm4(serverDefinition.getLocation().getAbsolutePath())) {
				version = JBPM4;
			}
			builder.append(version);
		}
		return builder.toString();
	}

	@Override
	public boolean exists(ServerDefinition serverDefinition) {
		if (serverDefinition == null || serverDefinition.getLocation() == null) {
			return false;
		}
		return jbpmExists(serverDefinition);
	}
}
