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
import java.util.ArrayList;
import java.util.List;

import org.drools.eclipse.util.DroolsRuntime;
import org.drools.eclipse.util.DroolsRuntimeManager;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.JBossRuntimeStartup.IJBossRuntimePersistanceHandler;
import org.jboss.tools.runtime.ServerDefinition;

public class DroolsHandler implements IJBossRuntimePersistanceHandler,IJBossRuntimePluginConstants {

	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		List<DroolsRuntime> droolsRuntimes = new ArrayList<DroolsRuntime>();
		for (ServerDefinition serverDefinition : serverDefinitions) {
			String type = serverDefinition.getType();
			if (!droolsExists(serverDefinition)) {
				if (SOA_P.equals(type) || DROOLS.equals(type)) {
					File droolsRoot = serverDefinition.getLocation(); //$NON-NLS-1$
					if (droolsRoot.isDirectory()) {
						DroolsRuntime runtime = new DroolsRuntime();
						if (SOA_P.equals(type)) {
							runtime.setName("Drools - " + serverDefinition.getName()); //$NON-NLS-1$
						} else {
							runtime.setName("Drools " + serverDefinition.getVersion()+ " - " + serverDefinition.getName()); //$NON-NLS-1$
						}
						runtime.setPath(droolsRoot.getAbsolutePath());
						DroolsRuntimeManager.recognizeJars(runtime);
						runtime.setDefault(true);
						droolsRuntimes.add(runtime);
					}
				}
			}
		}
		if (droolsRuntimes.size() > 0) {
			DroolsRuntime[] dra = droolsRuntimes.toArray(new DroolsRuntime[0]);
			DroolsRuntimeManager.setDroolsRuntimes(dra);
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

	public static String includeDrools(ServerDefinition serverDefinition) {
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
	public void importRuntimes() {
		// TODO Auto-generated method stub
		
	}

	public void exportRuntimes() {
		// TODO Auto-generated method stub
		
	}

}
