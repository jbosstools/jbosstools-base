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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonNavigator;
import org.jboss.tools.runtime.handlers.DroolsHandler;
import org.jboss.tools.runtime.handlers.JBossASHandler;
import org.jboss.tools.runtime.handlers.JbpmHandler;
import org.jboss.tools.runtime.handlers.SeamHandler;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;

public class JBossRuntimeStartup implements IStartup, IJBossRuntimePluginConstants {

	private static IJBossRuntimePersistanceHandler[] jBossRuntimePersistanceHandler = null;
	public static interface IJBossRuntimePersistanceHandler {
		public void initializeRuntimes(List<ServerDefinition> serverDefinitions);
		public void importRuntimes();
		public void exportRuntimes();
	}

	public static IJBossRuntimePersistanceHandler[] getPersistanceHandlers() {
		if (jBossRuntimePersistanceHandler == null) {
			jBossRuntimePersistanceHandler = new IJBossRuntimePersistanceHandler[] {
					new JbpmHandler(),
					new DroolsHandler(),
					new JBossASHandler(),
					new SeamHandler()
			};
		}
		return jBossRuntimePersistanceHandler;
	}
	
	private List<ServerDefinition> serverDefinitions = new ArrayList<ServerDefinition>();
	private IEclipsePreferences preferences;
	
	public void earlyStartup() {
		if (isJBDS() && willBeInitialized()) {
			parseServerFile();
			initializeRuntimes(serverDefinitions);
			saveWorkspacePreferences();
		}
	}
	
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		IJBossRuntimePersistanceHandler[] handlers = getPersistanceHandlers();
		for( int i = 0; i < handlers.length; i++ ) {
			handlers[i].initializeRuntimes(serverDefinitions);
		}
		refreshCommonNavigator();
	}
	
	private void saveWorkspacePreferences() {
		Activator.getDefault().getPreferenceStore().setValue(Activator.FIRST_START, false);
		String workspaces = getWorkspaces();
		String newWorkspaces = "";
		boolean addWorkspace = true;
		if (workspaces == null || workspaces.trim().length() == 0) {
			newWorkspaces = getWorkspace();
		} else {
			StringTokenizer tokenizer = new StringTokenizer(workspaces, ",");
			while (tokenizer.hasMoreTokens()) {
				String workspace = tokenizer.nextToken();
				if (workspace.equals(getWorkspace())) {
					addWorkspace = false;
				}
			}
			if (addWorkspace) {
				newWorkspaces = workspaces + "," + getWorkspace();
			}
		}
		if (addWorkspace) {
			IEclipsePreferences prefs = getPreferences();
			prefs.put(Activator.WORKSPACES, newWorkspaces);
			try {
				prefs.flush();
			} catch (BackingStoreException e) {
				Activator.log(e);
			}
		}
	}

	/**
	 * @return
	 */
	private boolean willBeInitialized() {
		boolean firstStart = Activator.getDefault().getPreferenceStore().getBoolean(Activator.FIRST_START);
		if (firstStart) {
			return true;
		}
	
		String workspaces = getWorkspaces();
		if (workspaces == null || workspaces.trim().length() == 0) {
			return true;
		}
		StringTokenizer tokenizer = new StringTokenizer(workspaces, ",");
		while (tokenizer.hasMoreTokens()) {
			String workspace = tokenizer.nextToken();
			if (workspace.equals(getWorkspace())) {
				return false;
			}
		}
		return true;
	}

	private String getWorkspaces() {
		IEclipsePreferences prefs = getPreferences();
		String workspaces = prefs.get(Activator.WORKSPACES, "");
		return workspaces;
	}

	private String getWorkspace() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IPath workspacePath = root.getLocation();
		return workspacePath.toOSString();
	}

	private IEclipsePreferences getPreferences() {
		if (preferences == null) {
			preferences = new ConfigurationScope().getNode(Activator.PLUGIN_ID);
		}
		return preferences;
	}

	private boolean isJBDS() {
		Bundle jbdsProduct = Platform.getBundle("com.jboss.jbds.product");
		return jbdsProduct != null;
	}

	private void refreshCommonNavigator() {
		// https://jira.jboss.org/jira/browse/JBDS-1091
		Display.getDefault().asyncExec(new Runnable() {
			
			public void run() {
				IViewPart view = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.wst.server.ui.ServersView");
				if (view instanceof CommonNavigator) {
					CommonNavigator navigator = (CommonNavigator) view;
					navigator.getCommonViewer().refresh();
				}
			}
		});
	}
	


	private void parseServerFile() {
		
		try {
			String pluginLocation = FileLocator.resolve(Activator.getDefault().getBundle().getEntry("/")).getPath(); //$NON-NLS-1$
			File serversFile = new File(pluginLocation, SERVERS_FILE);

			if (!serversFile.isFile()) {
				Location configLocation = Platform.getConfigurationLocation();
				URL configURL = configLocation.getURL();
				String configuration = FileLocator.resolve(configURL).getPath();
				serversFile = new File(configuration, SERVERS_FILE_CONFIGURATION).getCanonicalFile();
			} else {
				serversFile = serversFile.getCanonicalFile();
			}
			if (!serversFile.isFile()) {
				serversFile = new File(pluginLocation,SERVERS_FILE_NAME);
			}
			if (serversFile.isFile()) {
				//String str = FileUtil.readFile(serversFile);
				Properties servers = new Properties();
				servers.load(new BufferedInputStream(new FileInputStream(serversFile)));
				Enumeration<Object> elements = servers.elements();
				while (elements.hasMoreElements()) {
					String str = (String) elements.nextElement();
					StringTokenizer lineTokenizer = new StringTokenizer(str,
							"\n\r\f"); //$NON-NLS-1$
					while (lineTokenizer.hasMoreTokens()) {
						String lineToken = lineTokenizer.nextToken();
						StringTokenizer tokenizer = new StringTokenizer(
								lineToken, ","); //$NON-NLS-1$
						if (tokenizer.countTokens() == 4) {
							String name = tokenizer.nextToken();
							/*int index = name.indexOf('=');
							if (index < 0) {
								continue;
							}
							name = name.substring(index + 1);*/
							String type = tokenizer.nextToken();
							String version = tokenizer.nextToken();
							String location = tokenizer.nextToken();
							File locationFile = new File(location);
							if (locationFile.isDirectory()) {
								serverDefinitions.add(new ServerDefinition(
										name, version, type, locationFile));
							}
						}
					}
				}
			}
		} catch (FileNotFoundException e) {
			Activator.log(e);
		} catch (IOException e) {
			Activator.log(e);
		}
	}		
}