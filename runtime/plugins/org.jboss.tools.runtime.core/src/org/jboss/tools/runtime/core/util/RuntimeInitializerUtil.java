package org.jboss.tools.runtime.core.util;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
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
	public static void initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions) {
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(runtimeDefinitions);
			}
		}
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
