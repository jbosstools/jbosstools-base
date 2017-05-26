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
package org.jboss.tools.runtime.ui.internal.startup;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.osgi.service.datalocation.Location;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.RuntimeWorkbenchUtils;

/**
 * This class is only run on the first start of the product
 */
public class JBossRuntimeStartup {
	
	private static final String JBOSS_RUNTIMES = "jboss-runtimes"; //$NON-NLS-1$
	private static final String USER_HOME = "user.home"; //$NON-NLS-1$
	
	private static final String LOCATIONS_FILE_NAME = "runtime_locations.properties"; //$NON-NLS-1$
	private static final String LOCATIONS_FILE = "../../../" + LOCATIONS_FILE_NAME; //$NON-NLS-1$
	private static final String LOCATIONS_FILE_CONFIGURATION = "../" + LOCATIONS_FILE_NAME; //$NON-NLS-1$
	
	// The structure of jbds on mac 
	private static final String ROOT_RELATIVE_TO_CONFIG = (Platform.getOS().equals(Platform.OS_MACOSX) ? "../../../" : "");  //$NON-NLS-1$ //$NON-NLS-2$
	// JBoss EAP home directory (relative to configuration)
	private static final String JBOSS_EAP_HOME = ROOT_RELATIVE_TO_CONFIG + "../../runtimes/jboss-eap";  //$NON-NLS-1$
	// CDK root relative to configuration
	private static final String CDK_HOME = ROOT_RELATIVE_TO_CONFIG + "../../../cdk/"; //$NON-NLS-1$
	// CDK .minishift constant 
	private static final String DOT_MINISHIFT = ".minishift"; //$NON-NLS-1$

	
	public static void initializeRuntimes(IProgressMonitor monitor) {
		initializeEAPRuntimes(monitor);
		initializeCDKRuntimes(monitor);
		initializeRuntimesFromDefinitionFile(monitor);
		initializeUserHomeRuntimes(monitor);
		RuntimeWorkbenchUtils.refreshServersView();
	}
	
	private static void initializeUserHomeRuntimes(IProgressMonitor monitor) {
		String userHome = System.getProperty(USER_HOME);
		if (userHome != null) {
			File directory = new File(userHome, JBOSS_RUNTIMES);
			if (directory.isDirectory()) {
				RuntimeInitializerUtil.initializeRuntimesFromFolder(directory, monitor);
			}
			RuntimePath path = new RuntimePath(directory.getAbsolutePath());
			path.setScanOnEveryStartup(true);
			RuntimeUIActivator.getDefault().getModel().addRuntimePath(path);
		}
	}

	private static void initializeEAPRuntimes(IProgressMonitor monitor) {
		File directory = getEAPDirectory();
		RuntimeInitializerUtil.initializeRuntimesFromFolder(directory, monitor);
	}
	private static void initializeCDKRuntimes(IProgressMonitor monitor) {
		monitor.beginTask("Locating CDK Runtimes", 100); //$NON-NLS-1$
		File directory = getCDKDirectory();
		RuntimeInitializerUtil.initializeRuntimesFromFolder(directory, new SubProgressMonitor(monitor, 50));
		
		// cdk 3
		File directory2 = getCDK3Directory();
		RuntimePath path = new RuntimePath(directory2.getAbsolutePath());
		path.setScanOnEveryStartup(true);
		RuntimeUIActivator.getDefault().getModel().addRuntimePath(path);
		
		File directory3 = getCDK3MinishiftHomeDirectory();
		if( !directory2.equals(directory3) && directory3 != null) {
			RuntimePath path3 = new RuntimePath(directory3.getAbsolutePath());
			path3.setScanOnEveryStartup(true);
			RuntimeUIActivator.getDefault().getModel().addRuntimePath(path3);
		}
		

		RuntimeInitializerUtil.initializeRuntimesFromFolder(directory2, new SubProgressMonitor(monitor, 50));
		monitor.done();
	}
	
	private static void initializeRuntimesFromDefinitionFile(IProgressMonitor monitor) {
		final Set<RuntimePath> runtimePaths = parseRuntimeLocationsFile();
		for (RuntimePath runtimePath : runtimePaths) {
			RuntimeInitializerUtil.createRuntimeDefinitions(runtimePath, monitor);
		}
		
		if (runtimePaths.size() > 0) {
			HashSet<RuntimePath> set = new HashSet<RuntimePath>(Arrays.asList(RuntimeUIActivator.getDefault().getModel().getRuntimePaths()));
			set.addAll(runtimePaths);
			RuntimeUIActivator.getDefault().getModel().setRuntimePaths(set.toArray(new RuntimePath[set.size()]));
		}
	}

	private static File getEAPDirectory() {
		return getRelativeDirectory(JBOSS_EAP_HOME);
	}

	private static File getCDKDirectory() {
		return getRelativeDirectory(CDK_HOME);
	}

	private static File getCDK3Directory() {
		String home = System.getProperty("user.home");
		File homeF = new File(home);
		return new File(homeF, DOT_MINISHIFT);
	}
	private static File getCDK3MinishiftHomeDirectory() {
		String home = System.getenv("MINISHIFT_HOME");
		return home == null ? null : new File(home);
	}
	
	private static File getRelativeDirectory(String dir) {
		try {
			String configuration = getConfiguration();
			File directory = new File(configuration, dir);
			return directory;
		} catch( IOException ioe) {
			RuntimeUIActivator.pluginLog().logError(ioe);
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
			RuntimeUIActivator.pluginLog().logError(ioe);
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
