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
package org.jboss.tools.common.jdt.debug;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jdt.launching.IVMInstallType;
import org.eclipse.jdt.launching.JavaRuntime;
import org.jboss.tools.common.jdt.debug.tools.internal.IToolsConstants;

public class JavaUtilities {
	private JavaUtilities() {
		// Private constructor for static methods 
	}
	
	public static boolean isJigsawRunning() {
		String javaVersion = getJavaVersionSysprop();
		int[] majorMinor = getMajorMinor(javaVersion);
		return majorMinor[0] >= 9;
	}
	
	/**
	 * Accept a non-null string as per the following document:
	 * https://blogs.oracle.com/java-platform-group/a-new-jdk-9-version-string-scheme
	 * 
	 * Strings may or may not have all segments, however, the returned 
	 * array will be of length 2.  
	 * 
	 * @param version
	 * @return a integer array of length 2 representing the major+minor version of the string
	 */
	public static int[] getMajorMinor(String version) {
		return getMajorMinor(version, true);
	}
	
	/**
	 * Accept a non-null string as per the following document:
	 * https://blogs.oracle.com/java-platform-group/a-new-jdk-9-version-string-scheme
	 * 
	 * Strings may or may not have all segments, however, the returned 
	 * array will be of length 2.  
	 * 
	 * @param version
	 * @param log 
	 * @return a integer array of length 2 representing the major+minor version of the string
	 */
	public static int[] getMajorMinor(String version, boolean log) {

		if (version == null)
			return new int[] { -1, -1 };

		int pos = version.indexOf('.');
		if (pos == -1) {
			return new int[] { Integer.parseInt(version), 0 };
		}
		String[] split = version.split("\\.");
		if (split != null && split.length > 1) {
			try {
				return new int[] { Integer.parseInt(split[0]), Integer.parseInt(split[1]) };
			} catch (NumberFormatException nfe) {
				if (log) {
					RemoteDebugActivator.pluginLog().logError("Unable to parse java version string '" + version + "'",
							nfe);
				}
			}
		}
		return new int[] { -1, -1 };
	}
	
	public static String getJavaVersionVMInstall(IVMInstall install) {
		if( install instanceof IVMInstall2 ) {
			IVMInstall2 vmi2 = (IVMInstall2)install;
			return vmi2.getJavaVersion();
		}
		return null;
	}
	
	public static String getJavaVersionSysprop() {
		return System.getProperty("java.version");
	}
	
	
	/**
	 * Find the IVMInstall who's home directory matches the current java.home
	 * sysprop
	 * 
	 * @return
	 */
	public static IVMInstall findActiveVM() {
		String javaHome = System.getProperty(IToolsConstants.JAVA_HOME_PROPERTY_KEY);
		return findVMByInstallLocation(javaHome);
	}
	
	
	public static IVMInstall findVMByInstallLocation(String home) {
		if (home != null) {
			IVMInstallType[] types = JavaRuntime.getVMInstallTypes();
			for (int i = 0; i < types.length; i++) {
				IVMInstall[] installs = types[i].getVMInstalls();
				for (int j = 0; j < installs.length; j++) {
					File installLoc = installs[j].getInstallLocation();
					if (home.equals(installLoc.getAbsolutePath())) {
						return installs[j];
					}
				}
			}
		}
		return null;
	}
	
	
	public static IVMInstall findVMInstall(String id) {
		if (id != null) {
			IVMInstallType[] types = JavaRuntime.getVMInstallTypes();
			IVMInstall ret = null;
			for (int i = 0; i < types.length; i++) {
				ret = types[i].findVMInstall(id);
				if (ret != null)
					return ret;
			}
		}
		return null;
	}



	/**
	 * Does an IVMInstall currently exist for the currently-running java.home
	 * 
	 * @return
	 */
	public static boolean hasActiveVMInstall() {
		return findActiveVM() != null;
	}
	
	

	/**
	 * Gets the directories that could be JDK root directory.
	 * 
	 * @param javaHome
	 *            The java home path
	 * @return The directories that could be JDK root directory.
	 */
	public static List<File> getPossibleJdkRootDirectory(String javaHome) {
		List<File> dirs = new ArrayList<File>();

		/*
		 * On Mac, java home path can be for example:
		 * /Library/Java/JavaVirtualMachines/jdk1.7.0_13.jdk/Contents/Home/jre
		 */

		if (Platform.getOS().equals(Platform.OS_MACOSX)) {
			int index = javaHome.indexOf(IToolsConstants.JAVA_INSTALLATION_DIR_ON_MAC);
			if (index == -1) {
				return dirs;
			}

			String javaVirtualMachinesPath = javaHome.substring(0,
					index + IToolsConstants.JAVA_INSTALLATION_DIR_ON_MAC.length());
			File dir = new File(javaVirtualMachinesPath);

			collectDirs(dirs, dir, 3);
			return dirs;
		}

		File parentDir = new File(javaHome + File.separator + ".."); //$NON-NLS-1$
		// JRE's most often live inside a jdk's jre folder
		dirs.add(parentDir);
		if (parentDir.exists()) {
			for (File file : parentDir.listFiles()) {
				if (file.isDirectory()) {
					dirs.add(file);
				}
			}
		}

		return dirs;
	}
	
	
	/**
	 * Collects the directories which are within given depth from given base
	 * directory.
	 * 
	 * @param dirs
	 *            The directories to store result
	 * @param dir
	 *            The directory to search
	 * @param depth
	 *            The depth to search
	 */
	private static void collectDirs(List<File> dirs, File dir, int depth) {
		if (depth > 0) {
			for (File file : dir.listFiles()) {
				if (file.isDirectory()) {
					dirs.add(file);
					collectDirs(dirs, file, depth - 1);
				}
			}
		}
	}
	
	private static int compareJavaVersions(String version0, String version1) {
		int[] arg0majorMinor = getMajorMinor(version0, false);
		int[] arg1majorMinor = getMajorMinor(version1, false);
		if (arg0majorMinor[0] < arg1majorMinor[0])
			return -1;
		if (arg0majorMinor[0] > arg1majorMinor[0])
			return 1;
		if (arg0majorMinor[1] < arg1majorMinor[1])
			return -1;
		if (arg0majorMinor[1] > arg1majorMinor[1])
			return 1;
		return 0;
	}	
	
	/**
	 * Get all IVMInstalls that have a major.minor <= the currently running
	 * jre's major.minor
	 * 
	 * @return
	 */
	public static IVMInstall[] getAllCompatibleInstalls() {
		String currentVersion = System.getProperty("java.version");
		ArrayList<IVMInstall2> compat = new ArrayList<IVMInstall2>();
		IVMInstall[] all = getAllVMInstalls();
		for (int i = 0; i < all.length; i++) {
			if (all[i] instanceof IVMInstall2) {
				String vers = ((IVMInstall2) all[i]).getJavaVersion();
				// if running jre is greater than or equal the vm install, 
				// it's probably compatible
				if (compareJavaVersions(currentVersion, vers) >= 0) {
					compat.add((IVMInstall2) all[i]);
				}
			}
		}

		// Sort them by version number, so higher matches are at the beginning
		// of the list
		Comparator<IVMInstall2> comparator = new Comparator<IVMInstall2>() {
			public int compare(IVMInstall2 arg0, IVMInstall2 arg1) {
				String arg0vers = arg0.getJavaVersion();
				String arg1vers = arg1.getJavaVersion();
				return compareJavaVersions(arg0vers, arg1vers);
			}
		};

		Collections.sort(compat, comparator);
		Collections.reverse(compat);
		return compat.toArray(new IVMInstall[compat.size()]);
	}
	
	public static IVMInstall[] getAllVMInstalls() {
		ArrayList<IVMInstall> all = new ArrayList<IVMInstall>();
		for (IVMInstallType type : JavaRuntime.getVMInstallTypes()) {
			for (IVMInstall install : type.getVMInstalls()) {
				all.add(install);
			}
		}
		return all.toArray(new IVMInstall[all.size()]);
	}
}
