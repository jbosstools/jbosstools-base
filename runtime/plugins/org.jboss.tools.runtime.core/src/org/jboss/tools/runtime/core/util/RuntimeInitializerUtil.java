/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.util;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;

public class RuntimeInitializerUtil {
	public static void initializeRuntimesFromFolder(File directory, IProgressMonitor monitor) {
		if( directory != null && directory.isDirectory()) {
			RuntimePath runtimePath = new RuntimePath(directory.getAbsolutePath());
			List<RuntimeDefinition> runtimeDefinitions = createRuntimeDefinitions(runtimePath, monitor);
			initializeRuntimes(runtimeDefinitions);
		}
	}

	/**
	 * This is a long-running task which may involve bundle-loading. 
	 * This should not be called from the UI thread directly unless
	 * proper safety is taken. 
	 * 
	 * @param runtimeDefinitions
	 */
	public static IStatus initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions) {
		MultiStatus ms = new MultiStatus(RuntimeCoreActivator.PLUGIN_ID, 0, "Unable to initialize some runtime paths.", null);
		for( RuntimeDefinition def : runtimeDefinitions ) {
			boolean found = false;
			
			// If this runtime definition was created with a reference to its detector, just use that one detector
			try {
				if( def.getDetector() != null &&  def.getDetector().isEnabled()) {
					found |= def.getDetector().initializeRuntime(def);
				} else {
					Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
					for( IRuntimeDetector detector:detectors) {
						if (detector.isEnabled()) {
							found |= def.getDetector().initializeRuntime(def);
						}
					}
				}
			} catch(CoreException ce) {
				RuntimeCoreActivator.pluginLog().logError(ce);
			}
			if( !found ) {
				// Somehow display this error
				ms.add(RuntimeCoreActivator.statusFactory().errorStatus("All runtime detectors failed to initialize " + def.getName()));
			}
			
			/* Since we're only asking the runtime detector that created us, we may have
			   backward compatibility issues with ones that were expecting to be asked to initialize
			   a parent runtime and used that opportunity to create the child. 
			   
			   Ex: SeamHandler expected to be asked to initialize a jboss installation, so it can create the seam runtimes
			*/
			IStatus child = initializeRuntimes(def.getIncludedRuntimeDefinitions());
			if( !child.isOK())
				ms.add(child);
			
		}
		return ms;
	}
	
	/**
	 * This is a long-running task which may involve bundle-loading. 
	 * This should not be called from the UI thread directly unless
	 * proper safety is taken. 
	 * @param runtimePaths
	 * @param monitor
	 */
	public static void createRuntimeDefinitions(RuntimePath[] runtimePaths, IProgressMonitor monitor) {
		HashSet<RuntimePath> set = new HashSet<RuntimePath>(Arrays.asList(runtimePaths));
		createRuntimeDefinitions(set, monitor);
	}
	
	/**
	 * This is a long-running task which may involve bundle-loading. 
	 * This should not be called from the UI thread directly unless
	 * proper safety is taken. 
	 * @param runtimePaths
	 * @param monitor
	 */
	public static void createRuntimeDefinitions(Set<RuntimePath> runtimePaths, IProgressMonitor monitor) {
		for (RuntimePath runtimePath : runtimePaths) {
			createRuntimeDefinitions(runtimePath, monitor);
		}
	}
	
	/**
	 * This is a long-running task which may involve bundle-loading. 
	 * This should not be called from the UI thread directly unless
	 * proper safety is taken. 
	 * @param runtimePath
	 * @param monitor
	 * @return
	 */
	public static List<RuntimeDefinition> createRuntimeDefinitions(RuntimePath runtimePath, IProgressMonitor monitor) {
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		List<RuntimeDefinition> runtimeDefinitions = locator
				.searchForRuntimes(runtimePath.getPath(), monitor);
		for (RuntimeDefinition runtimeDefinition : runtimeDefinitions) {
			runtimeDefinition.setRuntimePath(runtimePath);
		}
		RuntimeDefinition[] defs = runtimeDefinitions.toArray(
				new RuntimeDefinition[runtimeDefinitions.size()]);
		runtimePath.setRuntimeDefinitions(defs);
		return runtimeDefinitions;
	}
}
