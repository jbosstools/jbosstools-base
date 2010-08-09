/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext.helper;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

/**
 * Helper for adding User Libraries (avoiding native dialogs)
 * 
 * @author jpeterka
 * 
 */
public class UserLibraryHelper {

	static Logger log = Logger.getLogger(UserLibraryHelper.class);

	/**
	 * Provides list of jars on given lib path
	 * 
	 * @param libPath
	 *            path to jar libraries
	 * @return
	 */
	public static String[] getJarList(String libPath) {
		File folder = new File(libPath);
		File[] listOfFiles = folder.listFiles();
		List<String> filePaths = new ArrayList<String>();

		for (int i = 0; i < listOfFiles.length; i++) {
			if (listOfFiles[i].isFile()
					&& listOfFiles[i].getName().endsWith(".jar")) {
				filePaths.add(listOfFiles[i].getAbsolutePath());
			}
		}
		String[] ret = new String[filePaths.size()];
		return filePaths.toArray(ret);
	}

	/**
	 * Creates user library with given identifiers and given jar list
	 * 
	 * @param libName
	 * @param jarPaths
	 */
	public static void addUserLibrary(final String libName,
			final String[] jarPaths) {

		ClasspathContainerInitializer initializer = JavaCore
				.getClasspathContainerInitializer(JavaCore.USER_LIBRARY_CONTAINER_ID);

		IPath containerPath = new Path(JavaCore.USER_LIBRARY_CONTAINER_ID);
		try {
			initializer.requestClasspathContainerUpdate(containerPath
					.append(libName), null, new IClasspathContainer() {

				public IClasspathEntry[] getClasspathEntries() {
					IClasspathEntry entry[] = new IClasspathEntry[jarPaths.length];
					for (int i = 0; i < entry.length; i++) {
						entry[i] = JavaCore.newLibraryEntry(new Path(
								jarPaths[i]), null, null);
					}
					return entry;
				}

				public String getDescription() {
					return libName;
				}

				public int getKind() {
					return K_APPLICATION;
				}

				public IPath getPath() {
					return new Path(JavaCore.USER_LIBRARY_CONTAINER_ID)
							.append(libName);
				}
			});
			log.info("User Library: " + libName + " defined");
		} catch (CoreException e) {
			log.error("Unable to add user library");
			log.error(e.getMessage());
		}
	}
}
