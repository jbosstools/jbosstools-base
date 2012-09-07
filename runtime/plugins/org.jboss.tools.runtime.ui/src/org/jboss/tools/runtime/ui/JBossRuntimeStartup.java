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

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonNavigator;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;

/**
 * This class is only run on the first start of the product
 */
public class JBossRuntimeStartup {
	
	private static final String JBOSS_EAP_HOME = "../../runtimes/jboss-eap"; 	// JBoss EAP home directory (relative to plugin)- <RHDS_HOME>/jbossas. //$NON-NLS-1$
	private static final String LOCATIONS_FILE_NAME = "runtime_locations.properties"; //$NON-NLS-1$
	private static final String LOCATIONS_FILE = "../../../../studio/" + LOCATIONS_FILE_NAME; //$NON-NLS-1$
	private static final String LOCATIONS_FILE_CONFIGURATION = "../../studio/" + LOCATIONS_FILE_NAME; //$NON-NLS-1$
	
	public static void initializeRuntimes(IProgressMonitor monitor) {
		initializeEAPRuntimes(monitor);
		initializeRuntimesFromDefinitionFile(monitor);
	}
	
	private static void initializeEAPRuntimes(IProgressMonitor monitor) {
		File directory = getEAPDirectory();
		RuntimeInitializerUtil.initializeRuntimesFromFolder(directory, monitor);
		RuntimeWorkbenchUtils.refreshServersView(); // ?!?!
	}
	
	private static void initializeRuntimesFromDefinitionFile(IProgressMonitor monitor) {
		final Set<RuntimePath> runtimePaths = parseRuntimeLocationsFile();
		for (RuntimePath runtimePath : runtimePaths) {
			RuntimeInitializerUtil.createRuntimeDefinitions(runtimePath, monitor);
		}
		
		if (runtimePaths.size() > 0) {
			RuntimeUIActivator.getDefault().getRuntimePaths().addAll(runtimePaths);
			RuntimeUIActivator.getDefault().saveRuntimePaths();
		}
	}

	private static File getEAPDirectory() {
		try {
			String configuration = getConfiguration();
			File directory = new File(configuration, JBOSS_EAP_HOME);
			return directory;
		} catch( IOException ioe) {
			RuntimeUIActivator.log(ioe);
		}
		return null;
	}

	private static File findRuntimeFile() {
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
				return serversFile;
			}
		} catch(IOException ioe) {
			RuntimeUIActivator.log(ioe);
		}
		return null;
	}
	
	private static Set<RuntimePath> parseRuntimeLocationsFile() {
		File runtimeFile = findRuntimeFile();
		if( runtimeFile != null && runtimeFile.isFile()) {
			return RuntimeModelUtil.parseRuntimeFile(runtimeFile);
		}
		return new TreeSet<RuntimePath>();
	}
	
	private static String getConfiguration() throws IOException {
		Location configLocation = Platform.getConfigurationLocation();
		URL configURL = configLocation.getURL();
		String configuration = FileLocator.resolve(configURL).getPath();
		return configuration;
	}

}