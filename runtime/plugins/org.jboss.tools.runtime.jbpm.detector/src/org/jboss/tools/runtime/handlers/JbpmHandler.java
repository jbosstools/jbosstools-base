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
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.jbpm.preferences.JbpmInstallation;
import org.jboss.tools.jbpm.preferences.PreferencesManager;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.ServerDefinition;

public class JbpmHandler extends AbstractRuntimeDetector {
	
	private static final String JBPM3 = "jBPM3"; //$NON-NLS-1$
	private static final String JBPM4 = "jBPM4"; //$NON-NLS-1$
	private static final String JBPM = "JBPM"; //$NON-NLS-1$
	private static final String SOA_P = "SOA-P"; //$NON-NLS-1$
	private static final String SOA_P_STD = "SOA-P-STD"; //$NON-NLS-1$
	
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
	
	@Override
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		for (ServerDefinition serverDefinition : serverDefinitions) {
			if (serverDefinition.isEnabled() && !jbpmExists(serverDefinition)) {
				File jbpmRoot = getJbpmRoot(serverDefinition);
				if (jbpmRoot == null || !jbpmRoot.isDirectory()) {
					continue;
				}
				String type = serverDefinition.getType();
				if (JBPM.equals(type)) {
					PreferencesManager.getInstance().addJbpmInstallation(serverDefinition.getName(), jbpmRoot.getAbsolutePath(), serverDefinition.getVersion());
				}
			}
			initializeRuntimes(serverDefinition.getIncludedServerDefinitions());
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

	@Override
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
			String name = root.getName();
			int index = 1;
			boolean nameExists = PreferencesManager.getInstance().getJbpmInstallation(name) != null;
			while (nameExists) {
				name = root.getName() + " " + index++;
				nameExists = PreferencesManager.getInstance().getJbpmInstallation(name) != null;
			}
			return new ServerDefinition(name, version, JBPM, root.getAbsoluteFile());
		}
		return null;
	}
	
	private static boolean isJbpm3(String location) {
		return new Path(location).append("/src/resources/gpd/version.info.xml").toFile().exists();
	}
	
	private static boolean isJbpm4(String location) {
		return new Path(location).append("/jbpm.jar").toFile().exists();
	}
	
	private boolean isValidJbpmInstallation(String location) {
		return isJbpm3(location) || isJbpm4(location);
	}
	
	@Override
	public boolean exists(ServerDefinition serverDefinition) {
		if (serverDefinition == null || serverDefinition.getLocation() == null) {
			return false;
		}
		return jbpmExists(serverDefinition);
	}

	public static void calculateIncludedServerDefinition(
			ServerDefinition serverDefinition) {
		if (serverDefinition == null || !SOA_P.equals(serverDefinition.getType())) {
			return;
		}
		File jbpmRoot = new File(serverDefinition.getLocation(),"jbpm-jpdl"); //$NON-NLS-1$
		if (jbpmRoot.isDirectory()) {
			String version = JBPM3;
			if (isJbpm4(serverDefinition.getLocation().getAbsolutePath())) {
				version = JBPM4;
			}
			ServerDefinition sd = new ServerDefinition(serverDefinition.getName(), version, JBPM, jbpmRoot);
			sd.setParent(serverDefinition);
			serverDefinition.getIncludedServerDefinitions().add(sd);
		}
	}
}
