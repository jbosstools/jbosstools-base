/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime.core;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.ServerDefinition;

/**
 * @author snjeza
 *
 */
public class JBossRuntimeLocator {

	private static final int DEPTH = 4;

	public JBossRuntimeLocator() {
	}
	
	public List<ServerDefinition> searchForRuntimes(String path, IProgressMonitor monitor) {
		return searchForRuntimes(new Path(path), monitor);
	}

	public List<ServerDefinition> searchForRuntimes(IPath path, IProgressMonitor monitor) {
		List<ServerDefinition> serverDefinitions = new ArrayList<ServerDefinition>();
		searchForRuntimes(serverDefinitions, path, monitor);
		return serverDefinitions;
	}
	
	private void searchForRuntimes(List<ServerDefinition> serverDefinitions, IPath path, 
			IProgressMonitor monitor) {
		File[] files = null;
		if (path != null) {
			File root = path.toFile();
			if (root.isDirectory())
				files = new File[] { root };
			else
				return;
		} else
			files = File.listRoots();

		if (files != null) {
			int size = files.length;
			int work = 100 / size;
			int workLeft = 100 - (work * size);
			for (int i = 0; i < size; i++) {
				if (monitor.isCanceled())
					return;
				if (files[i] != null && files[i].isDirectory())
					searchDirectory(files[i], serverDefinitions, DEPTH, monitor);
				monitor.worked(work);
			}
			monitor.worked(workLeft);
		} else {
			monitor.worked(100);
		}
		
	}
	
	private void searchDirectory(File directory, List<ServerDefinition> serverDefinitions,
			int depth, IProgressMonitor monitor) {
		if (depth == 0 || monitor.isCanceled() || directory == null || !directory.isDirectory()) {
			return;
		}
		
		monitor.setTaskName("Searching " + directory.getAbsolutePath());
		
		Set<IRuntimeDetector> runtimeDetectors = RuntimeCoreActivator.getRuntimeDetectors();
		for (IRuntimeDetector detector:runtimeDetectors) {
			if (monitor.isCanceled()) {
				return;
			}
			if (!detector.isEnabled()) {
				continue;
			}
			ServerDefinition serverDefinition = detector.getServerDefinition(directory, monitor);
			if (serverDefinition != null) {
				serverDefinitions.add(serverDefinition);
				return;
			}
		}
		
		File[] files = directory.listFiles(new FileFilter() {
			public boolean accept(File file) {
				return file.isDirectory();
			}
		});
		if (files != null) {
			int size = files.length;
			for (int i = 0; i < size; i++) {
				if (monitor.isCanceled())
					return;
				searchDirectory(files[i], serverDefinitions, depth - 1, monitor);
			}
		}
	}

}
