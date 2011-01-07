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
package org.jboss.tools.runtime.core;

import java.io.File;
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
		Set<IRuntimeDetector> runtimeDetectors = RuntimeCoreActivator.getRuntimeDetectors();
		return searchForRuntimes(serverDefinitions, path, DEPTH, runtimeDetectors, monitor);
	}
	
	private List<ServerDefinition> searchForRuntimes(List<ServerDefinition> serverDefinitions, IPath path, 
			int depth, Set<IRuntimeDetector> runtimeDetectors, IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return serverDefinitions;
		}
		File[] children = null;
		if (path != null) {
			File root = path.toFile();
			monitor.setTaskName("Searching " + path.toOSString());
			ServerDefinition serverDefinition = null;
			for (IRuntimeDetector detector:runtimeDetectors) {
				if (monitor.isCanceled()) {
					return serverDefinitions;
				}
				if (!detector.isEnabled()) {
					continue;
				}
				serverDefinition = detector.getServerDefinition(root, monitor);
				if (serverDefinition != null) {
					serverDefinitions.add(serverDefinition);
					break;
				}
			}
			if (serverDefinition == null) {
				children = root.listFiles();
			}
		} else {
			children = File.listRoots();
		}
		
		if (monitor.isCanceled()) {
			return serverDefinitions;
		}
		if (depth == 0) {
			return serverDefinitions; 
		}
		if( children != null ) {
			for( int i = 0; i < children.length; i++ ) {
				if (monitor.isCanceled()) {
					return serverDefinitions;
				}
				if( children[i].isDirectory()) {
					if (monitor.isCanceled()) {
						return serverDefinitions;
					}
					searchForRuntimes(serverDefinitions, new Path(children[i].getAbsolutePath()),
							--depth, runtimeDetectors, monitor);
				}
			}
		}
		return serverDefinitions;
	}

}
