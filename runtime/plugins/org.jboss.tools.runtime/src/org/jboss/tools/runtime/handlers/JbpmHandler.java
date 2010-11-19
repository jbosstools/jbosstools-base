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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.wst.server.core.internal.IMemento;
import org.eclipse.wst.server.core.internal.XMLMemento;
import org.jboss.tools.jbpm.preferences.JbpmInstallation;
import org.jboss.tools.jbpm.preferences.PreferencesManager;
import org.jboss.tools.runtime.Activator;
import org.jboss.tools.runtime.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.JBossRuntimeLocator;
import org.jboss.tools.runtime.JBossRuntimeStartup.IJBossRuntimePersistanceHandler;
import org.jboss.tools.runtime.ServerDefinition;

public class JbpmHandler implements IJBossRuntimePersistanceHandler,IJBossRuntimePluginConstants {
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		for (ServerDefinition serverDefinition : serverDefinitions) {
			if (!jbpmExists(serverDefinition)) {
				String type = serverDefinition.getType();
				if (SOA_P.equals(type)) {
					File jbpmRoot = new File(serverDefinition.getLocation(),"jbpm-jpdl"); //$NON-NLS-1$
					if (jbpmRoot.isDirectory()) {
						String version = JBossRuntimeLocator.JBPM3;
						if (JBossRuntimeLocator.isJbpm4(serverDefinition.getLocation().getAbsolutePath())) {
							version = JBossRuntimeLocator.JBPM4;
						}
						PreferencesManager.getInstance().initializeDefaultJbpmInstallation(serverDefinition.getName(), jbpmRoot.getAbsolutePath(), version);
					}
				} else if (JBPM.equals(type)) {
					PreferencesManager.getInstance().addJbpmInstallation(serverDefinition.getName(), serverDefinition.getLocation().getAbsolutePath(), serverDefinition.getVersion());
				}
			}
		}
		
	}
	
	/**
	 * @param serverDefinition
	 * @return
	 */
	public static boolean jbpmExists(ServerDefinition serverDefinition) {
		Map<String, JbpmInstallation> jbpmMap = PreferencesManager.getInstance().getJbpmInstallationMap();
		Collection<JbpmInstallation> jbpmInstalations = jbpmMap.values();
		for (JbpmInstallation jbpm:jbpmInstalations) {
			String location = jbpm.location;
			if (location != null && location.equals(serverDefinition.getLocation().getAbsolutePath())) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * @param jbpms
	 */
	public static void loadJBPMInstalations(String jbpms) {
		InputStream in = null;
		try {
			in = new ByteArrayInputStream(jbpms.getBytes("UTF-8"));
			IMemento memento = XMLMemento.loadMemento(in);
			IMemento[] children = memento.getChildren("installation");
			for (int i = 0; i < children.length; i++) {
				JbpmInstallation installation = new JbpmInstallation();
				installation.name = children[i].getString("name");
				installation.location = children[i].getString("location");
				installation.version = children[i].getString("version");
				PreferencesManager.getInstance().getJbpmInstallationMap()
						.put(installation.name, installation);
			}
		} catch (Exception e) {
			Activator.log(e);
		} finally {
			try {
				if (in != null) {
					in.close();
				}
			} catch (IOException e) {
			}
		}
	}

	public static void exportJbpms() {
		File file = org.jboss.tools.jbpm.Activator.getDefault().getStateLocation().append("jbpm-installations.xml").toFile();
		if (!file.exists()) {
			Activator.getDefault().getPreferenceStore().setValue(Activator.JBPMS, "");
			return;
		}
		try {
			XMLMemento memento = (XMLMemento) XMLMemento.loadMemento(file.getAbsolutePath());
			String xmlString = memento.saveToString();
			Activator.getDefault().getPreferenceStore().setValue(Activator.JBPMS, xmlString);
			Activator.getDefault().savePluginPreferences();
		} catch (Exception e) {
			Activator.log (e);
		}
	}

	public static String includeJbpm(ServerDefinition serverDefinition) {
		StringBuilder builder = new StringBuilder();
		File jbpmRoot = new File(serverDefinition.getLocation(),"jbpm-jpdl"); //$NON-NLS-1$
		if (jbpmRoot.isDirectory()) {
			String version = JBossRuntimeLocator.JBPM3;
			if (JBossRuntimeLocator.isJbpm4(serverDefinition.getLocation().getAbsolutePath())) {
				version = JBossRuntimeLocator.JBPM4;
			}
			builder.append(version);
		}
		return builder.toString();
	}
	
	public void importRuntimes() {
		String jbpms = Activator.getDefault().getPreferenceStore().getString(Activator.JBPMS);
		if (jbpms != null && jbpms.trim().length() > 0) {
			JbpmHandler.loadJBPMInstalations(jbpms);
		}
	}

	public void exportRuntimes() {
		JbpmHandler.exportJbpms();
	}

}
