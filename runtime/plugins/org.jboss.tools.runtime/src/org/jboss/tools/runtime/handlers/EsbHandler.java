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
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.esb.core.runtime.JBossESBRuntime;
import org.jboss.tools.esb.core.runtime.JBossRuntimeManager;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.ServerDefinition;

public class EsbHandler extends AbstractRuntimeDetector implements IJBossRuntimePluginConstants {

	private static final String DEFAULT_CONFIGURATION = "default";

	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		for (ServerDefinition serverDefinition : serverDefinitions) {
			String type = serverDefinition.getType();
			if (serverDefinition.isEnabled() && !esbExists(serverDefinition)) {
				if (SOA_P.equals(type) || SOA_P_STD.equals(type)) {
					File esbRoot;
					if (SOA_P.equals(type)) {
						esbRoot = serverDefinition.getLocation();
					} else {
						esbRoot = new File(serverDefinition.getLocation(), "jboss-esb"); //$NON-NLS-1$
					}
					if (esbRoot.isDirectory()) {
						JBossESBRuntime runtime = new JBossESBRuntime();
						runtime.setName("ESB - " + serverDefinition.getName()); //$NON-NLS-1$
						runtime.setHomeDir(esbRoot.getAbsolutePath());
						runtime.setConfiguration(DEFAULT_CONFIGURATION);
						String version = JBossRuntimeManager.getInstance().getVersion(esbRoot.getAbsolutePath(), DEFAULT_CONFIGURATION);
						runtime.setVersion(version);
						JBossRuntimeManager.getInstance().addRuntime(runtime);
					}
				}
			}
		}
		
	}

	/**
	 * @param serverDefinition
	 * @return
	 */
	private static boolean esbExists(ServerDefinition serverDefinition) {
		JBossESBRuntime[] runtimes = JBossRuntimeManager.getInstance().getRuntimes();
		for (JBossESBRuntime runtime:runtimes) {
			String location = runtime.getHomeDir();
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
		// standalone ESB runtime
		return null;
	}
	
	public static String included(ServerDefinition serverDefinition) {
		StringBuilder builder = new StringBuilder();
		File esbRoot = serverDefinition.getLocation(); //$NON-NLS-1$
		if (esbRoot.isDirectory()) {
			builder.append("ESB");
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
		return esbExists(serverDefinition);
	}
}
