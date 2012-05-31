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
package org.jboss.tools.runtime.ui;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonNavigator;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;

public class JBossRuntimeStartup {
	
	private static final String JBOSS_EAP_HOME = "../../jboss-eap"; 	// JBoss EAP home directory (relative to plugin)- <RHDS_HOME>/jbossas. //$NON-NLS-1$
	private static final String LOCATIONS_FILE_NAME = "runtime_locations.properties"; //$NON-NLS-1$
	private static final String LOCATIONS_FILE = "../../../../studio/" + LOCATIONS_FILE_NAME; //$NON-NLS-1$
	private static final String LOCATIONS_FILE_CONFIGURATION = "../../studio/" + LOCATIONS_FILE_NAME; //$NON-NLS-1$
	
	public static void initializeRuntimes(IProgressMonitor monitor) {
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		try {
			String configuration = getConfiguration();
			File directory = new File(configuration, JBOSS_EAP_HOME);
			if (directory.isDirectory()) {
				RuntimePath runtimePath = new RuntimePath(
						directory.getAbsolutePath());
				List<RuntimeDefinition> serverDefinitions = locator
						.searchForRuntimes(runtimePath.getPath(), monitor);
				runtimePath.getServerDefinitions().clear();
				for (RuntimeDefinition serverDefinition : serverDefinitions) {
					serverDefinition.setRuntimePath(runtimePath);
				}
				initializeRuntimes(serverDefinitions);
			}
		} catch (IOException e) {
			RuntimeUIActivator.log(e);
		}
		final Set<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
		parseRuntimeLocationsFile(runtimePaths);
		for (RuntimePath runtimePath : runtimePaths) {
			List<RuntimeDefinition> serverDefinitions = locator
					.searchForRuntimes(runtimePath.getPath(), monitor);
			runtimePath.getServerDefinitions().clear();
			for (RuntimeDefinition serverDefinition : serverDefinitions) {
				serverDefinition.setRuntimePath(runtimePath);
			}
			runtimePath.getServerDefinitions().addAll(serverDefinitions);
		}
		if (runtimePaths.size() > 0) {
			RuntimeUIActivator.getDefault().getRuntimePaths()
					.addAll(runtimePaths);
			RuntimeUIActivator.getDefault().saveRuntimePaths();
		}
	}

	public static void initializeRuntimes(List<RuntimeDefinition> serverDefinitions) {
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getRuntimeDetectors();
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(serverDefinitions);
			}
		}
		refreshCommonNavigator();
	}

	private static void refreshCommonNavigator() {
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
	
	private static void parseRuntimeLocationsFile(Set<RuntimePath> runtimePaths) {
		
		try {
			String pluginLocation = FileLocator.resolve(RuntimeUIActivator.getDefault().getBundle().getEntry("/")).getPath(); //$NON-NLS-1$
			File serversFile = new File(pluginLocation, LOCATIONS_FILE);

			if (!serversFile.isFile()) {
				String configuration = getConfiguration();
				serversFile = new File(configuration, LOCATIONS_FILE_CONFIGURATION).getCanonicalFile();
			} else {
				serversFile = serversFile.getCanonicalFile();
			}
			if (!serversFile.isFile()) {
				serversFile = new File(pluginLocation,LOCATIONS_FILE_NAME);
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
						if (tokenizer.countTokens() == 2) {
							String location = tokenizer.nextToken();
							boolean scan = Boolean.parseBoolean(tokenizer.nextToken());
							File locationFile = new File(location);
							if (locationFile.isDirectory()) {
								RuntimePath tempLocation = new RuntimePath(location);
								tempLocation.setScanOnEveryStartup(scan);
								runtimePaths.add(tempLocation);
							}
						}
					}
				}
			}
		} catch (FileNotFoundException e) {
			RuntimeUIActivator.log(e);
		} catch (IOException e) {
			RuntimeUIActivator.log(e);
		}
	}

	private static String getConfiguration() throws IOException {
		Location configLocation = Platform.getConfigurationLocation();
		URL configURL = configLocation.getURL();
		String configuration = FileLocator.resolve(configURL).getPath();
		return configuration;
	}

}