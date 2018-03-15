/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.tools.internal;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jdt.launching.IVMInstall;
import org.jboss.tools.common.jdt.debug.IPropertyKeys;
import org.jboss.tools.common.jdt.debug.JavaUtilities;
import org.jboss.tools.common.jdt.debug.Messages;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

public class PreJigsawUtility {

	public File getToolsJarFile() {
		File toolsJar = getToolsJar();
		if (toolsJar != null) { 
			try {
				toolsJar = toolsJar.getCanonicalFile();
				if (toolsJar.exists()) {
					return toolsJar;
				}
			} catch (IOException e1) {
				return null;
			}
		}
		return null;
	}	
	
	public URLClassLoader getToolsJarClassloader(File file, ClassLoader parent) {
		URL toolsUrl;
		try {
			toolsUrl = file.toURI().toURL();
			return new URLClassLoader(new URL[] { toolsUrl }, parent);
		} catch (MalformedURLException e) {
			return null;
		}
	}
	
	private File getToolsJar() {
		// Find the jdk root for the currently running java home
		String jdkRootDirectory = findHomeDirectoryToAddToClasspath();
		if( jdkRootDirectory != null )
			return new File(jdkRootDirectory, IToolsConstants.TOOLS_JAR);
		return null;
	}
	

	private String findHomeDirectoryToAddToClasspath() {
		// Find the jdk root for the currently running java home
		String jdkRootDirectory = findJdkRootFromJavaHome();
		if (jdkRootDirectory == null || jdkRootDirectory.trim().isEmpty()) {
			IVMInstall vmi = findSecondaryVMInstall();
			if (vmi != null)
				jdkRootDirectory = vmi.getInstallLocation().getAbsolutePath();
		}
		return jdkRootDirectory;
	}


	/**
	 * Get the vminstall to use when the currently-running jre is not a jdk
	 * 
	 * @return
	 */
	public IVMInstall findSecondaryVMInstall() {
		IVMInstall vmi = findHomeDirFromPreferences();
		if (vmi == null) {
			vmi = findFirstValidVMInstall();
		}
		return vmi;
	}
	

	// Find the home directory based on the preference settings
	private IVMInstall findHomeDirFromPreferences() {
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(RemoteDebugActivator.PLUGIN_ID);
		String vm = prefs.get(IPropertyKeys.JDK_VM_INSTALL, null);
		if (vm != null) {
			IVMInstall found = JavaUtilities.findVMInstall(vm);
			if (found != null) {
				return found;
			}
		}
		return null;
	}

	/**
	 * Find the first vm-install that is valid
	 * 
	 * @return The JDK root directory, or empty string if not found
	 */
	private IVMInstall findFirstValidVMInstall() {
		// search from the JREs that are compatible based on java version
		IVMInstall[] all = JavaUtilities.getAllCompatibleInstalls();
		for (int i = 0; i < all.length; i++) {
			String jdkRootDirectory = all[i].getInstallLocation().getPath();
			if (null == validateJdkRootDirectory(jdkRootDirectory)) {
				return all[i];
			}
		}
		RemoteDebugActivator.pluginLog().logMessage(IStatus.WARNING, Messages.jdkRootDirectoryNotFoundMsg,
				new Exception(Messages.jdkRootDirectoryNotFoundMsg));
		return null; 
	}
	

	public String findJdkRootFromJavaHome() {
		// search at the same directory as current JRE
		String javaHome = System.getProperty(IToolsConstants.JAVA_HOME_PROPERTY_KEY);
		for (File directory : JavaUtilities.getPossibleJdkRootDirectory(javaHome)) {
			String path = directory.getPath();
			// If there are no errors in validating the path... 
			if (null == validateJdkRootDirectory(path)) {
				return path;
			}
		}
		return null;
	}


	/**
	 * Validates the JDK root directory.
	 * 
	 * @param jdkRootDirectory
	 *            The JDK root directory
	 * @return The error message or <tt>null</tt> if not found
	 */
	public String validateJdkRootDirectory(String jdkRootDirectory) {
		if (jdkRootDirectory == null || jdkRootDirectory.trim().isEmpty()) {
			return Messages.NoJdkDirectoryFoundMsg;
		}
		// check if directory exists
		File directory = new File(jdkRootDirectory);
		if (!directory.exists() || !directory.isDirectory()) {
			return Messages.directoryNotExistMsg;
		}

		// check if tools.jar exists
		File toolsJarFile = new File(jdkRootDirectory + IToolsConstants.TOOLS_JAR);
		if (!toolsJarFile.exists()) {
			return Messages.notJdkRootDirectoryMsg;
		}

		// checks if "attach" shared library exist
		String libraryPath = getJreLibraryPath(jdkRootDirectory);
		if (libraryPath == null) {
			return Messages.notJdkRootDirectoryMsg;
		}
		return null;
	}

	/**
	 * Gets the JRE library path.
	 * 
	 * @param jdkRootDirectory
	 *            The JDK root directory
	 * @return The JRE library path or <tt>null</tt> it not found
	 */
	private static String getJreLibraryPath(String jdkRootDirectory) {
		for (String path : IToolsConstants.LIBRARY_PATHS) {
			File attachLibraryFile = new File(jdkRootDirectory + path + File.separator
					+ System.mapLibraryName(IToolsConstants.ATTACH_LIBRARY));
			if (attachLibraryFile.exists()) {
				return jdkRootDirectory + path;
			}
		}
		return null;
	}


}
