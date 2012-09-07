package org.jboss.tools.runtime.core.util;

import java.io.File;
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

	public static void initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions) {
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(runtimeDefinitions);
			}
		}
	}
	
	public static void createRuntimeDefinitions(Set<RuntimePath> runtimePaths, IProgressMonitor monitor) {
		for (RuntimePath runtimePath : runtimePaths) {
			// TODO sub monitors
			createRuntimeDefinitions(runtimePath, monitor);
		}
	}
	
	public static List<RuntimeDefinition> createRuntimeDefinitions(RuntimePath runtimePath, IProgressMonitor monitor) {
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		List<RuntimeDefinition> runtimeDefinitions = locator
				.searchForRuntimes(runtimePath.getPath(), monitor);
		runtimePath.getRuntimeDefinitions().clear();
		for (RuntimeDefinition runtimeDefinition : runtimeDefinitions) {
			runtimeDefinition.setRuntimePath(runtimePath);
		}
		runtimePath.getRuntimeDefinitions().addAll(runtimeDefinitions);
		return runtimeDefinitions;
	}
}
