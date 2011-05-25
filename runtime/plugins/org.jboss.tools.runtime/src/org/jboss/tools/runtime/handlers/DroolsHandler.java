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
package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;

import org.drools.eclipse.util.DroolsRuntime;
import org.drools.eclipse.util.DroolsRuntimeManager;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.ServerDefinition;
import org.osgi.framework.Bundle;

public class DroolsHandler extends AbstractRuntimeDetector implements IJBossRuntimePluginConstants {

	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		DroolsRuntime[] existingRuntimes = DroolsRuntimeManager.getDroolsRuntimes();
		List<DroolsRuntime> droolsRuntimes = new ArrayList<DroolsRuntime>();
		if (existingRuntimes != null) {
			for (DroolsRuntime runtime:existingRuntimes) {
				droolsRuntimes.add(runtime);
			}
		}
		initializeInternal(serverDefinitions, droolsRuntimes);
		if (droolsRuntimes.size() > 0) {
			DroolsRuntime[] dra = droolsRuntimes.toArray(new DroolsRuntime[0]);
			DroolsRuntimeManager.setDroolsRuntimes(dra);
		}
		
	}

	private void initializeInternal(List<ServerDefinition> serverDefinitions,
			List<DroolsRuntime> droolsRuntimes) {
		for (ServerDefinition serverDefinition : serverDefinitions) {
			String type = serverDefinition.getType();
			if (serverDefinition.isEnabled() && !droolsExists(serverDefinition)) {
				if (DROOLS.equals(type)) {
					File droolsRoot = serverDefinition.getLocation(); //$NON-NLS-1$
					if (droolsRoot.isDirectory()) {
						DroolsRuntime runtime = new DroolsRuntime();
						runtime.setName("Drools " + serverDefinition.getVersion()+ " - " + serverDefinition.getName()); //$NON-NLS-1$
						runtime.setPath(droolsRoot.getAbsolutePath());
						DroolsRuntimeManager.recognizeJars(runtime);
						runtime.setDefault(true);
						droolsRuntimes.add(runtime);
					}
				}
			}
			initializeInternal(serverDefinition.getIncludedServerDefinitions(), droolsRuntimes);
		}
	}

	/**
	 * @param serverDefinition
	 * @return
	 */
	private static boolean droolsExists(ServerDefinition serverDefinition) {
		DroolsRuntime[] droolsRuntimes = DroolsRuntimeManager.getDroolsRuntimes();
		for (DroolsRuntime dr:droolsRuntimes) {
			String location = dr.getPath();
			if (location != null && location.equals(serverDefinition.getLocation().getAbsolutePath())) {
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
		String[] files = root.list(new FilenameFilter() {
			
			public boolean accept(File dir, String name) {
				if (name.startsWith("drools-api") && name.endsWith(".jar") ) {
					return true;
				}
				return false;
			}
		});
		if (files != null && files.length > 0) {
			String version = getImplementationVersion(root,files[0]);
			if (version != null) {
				version = version.substring(0,3);
				return new ServerDefinition(root.getName(), version, DROOLS, root.getAbsoluteFile());
			}
		}
		return null;
	}
	
	public static String included(ServerDefinition serverDefinition) {
		StringBuilder builder = new StringBuilder();
		File droolsRoot = serverDefinition.getLocation(); //$NON-NLS-1$
		if (droolsRoot.isDirectory()) {
			builder.append("Drools");
			if (serverDefinition.getVersion() != null && serverDefinition.getVersion().length() > 0) {
				builder.append(" ");
				builder.append(serverDefinition.getVersion());
			}
		}
		return builder.toString();
	}

	@Override
	public boolean exists(ServerDefinition serverDefinition) {
		if (serverDefinition == null || serverDefinition.getLocation() == null) {
			return false;
		}
		return droolsExists(serverDefinition);
	}

	public static void calculateIncludedServerDefinition(
			ServerDefinition serverDefinition) {
		if (serverDefinition == null || !SOA_P.equals(serverDefinition.getType())) {
			return;
		}
		File droolsRoot = serverDefinition.getLocation(); //$NON-NLS-1$
		if (droolsRoot.isDirectory()) {
			String name = "Drools - " + serverDefinition.getName(); //$NON-NLS-1$
			ServerDefinition sd = new ServerDefinition(name, serverDefinition.getVersion(), DROOLS, droolsRoot);
			sd.setParent(serverDefinition);
			serverDefinition.getIncludedServerDefinitions().add(sd);
		}
	}

	@Override
	public boolean isValid() {
		if (super.isValid()) {
			Bundle droolsBundle = Platform.getBundle("org.drools.eclipse"); //$NON-NLS-1$
			return droolsBundle != null;
		} 
		return false;
	}
}
