package org.jboss.tools.runtime.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.jboss.tools.common.xml.IMemento;
import org.jboss.tools.common.xml.XMLMemento;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;

public class RuntimePathPreferenceIO {
	private static final String RUNTIME_PATHS = "runtimePaths";
	private static final String PATH = "path";
	private static final String RUNTIME_PATH = "runtimePath";
	private static final String SCAN_ON_EVERY_STAERTUP = "scanOnEveryStartup";
	private static final String TIMESTAMP = "timestamp";
	private static final String SERVER_DEFINITIONS = "serverDefinitions";
	private static final String SERVER_DEFINITION = "serverDefinition";
	private static final String NAME = "name";
	private static final String INCLUDED_DEFINITION = "included";
	private static final String VERSION = "version";
	private static final String TYPE = "type";
	private static final String LOCATION = "location";
	private static final String DESCRIPTION = "description";
	private static final String ENABLED = "enabled";
	public static final String FIRST_START = "firstStart"; //$NON-NLS-1$
	public static final String PREFERENCES_VERSION = "version"; //$NON-NLS-1$
	private static final String RUNTIME_PREFERENCES_VERSION = "2"; //$NON-NLS-1$
	
	public static String getPreferenceOutputString(Set<RuntimePath> runtimePaths) throws IOException {
		XMLMemento memento = XMLMemento.createWriteRoot(RUNTIME_PATHS);
		memento.putString(PREFERENCES_VERSION, RUNTIME_PREFERENCES_VERSION);
		for (RuntimePath runtimePath:runtimePaths) {
			IMemento runtimePathNode = memento.createChild(RUNTIME_PATH);
			runtimePathNode.putString(PATH, runtimePath.getPath());
			runtimePathNode.putBoolean(SCAN_ON_EVERY_STAERTUP, runtimePath.isScanOnEveryStartup());
			runtimePathNode.putString(TIMESTAMP, String.valueOf(runtimePath.getTimestamp()));
			IMemento runtimeDefinitionsNode = runtimePathNode.createChild(SERVER_DEFINITIONS);
			RuntimeDefinition[] definitions = runtimePath.getRuntimeDefinitions();
			putDefinitions(runtimeDefinitionsNode, definitions);	
		}
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		BufferedOutputStream os = new BufferedOutputStream(baos);
		try {
			memento.save(os);
			os.flush();
			String runtimes = baos.toString();
			return runtimes;
		} finally {
			if (os != null) {
				try {
					os.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
	}
	
	private static void putDefinitions(IMemento runtimeDefintionsNode,
			RuntimeDefinition[] definitions) {
		for (RuntimeDefinition runtimeDefinition:definitions) {
			IMemento sdNode = runtimeDefintionsNode.createChild(SERVER_DEFINITION);
			putRuntimeDefinition(runtimeDefinition, sdNode);
			IMemento includedNodes = sdNode.createChild(INCLUDED_DEFINITION);
			for (RuntimeDefinition included:runtimeDefinition.getIncludedRuntimeDefinitions()) {
				IMemento includedNode = includedNodes.createChild(SERVER_DEFINITION);
				putRuntimeDefinition(included, includedNode);
			}
		}
	}
	
	private static void putRuntimeDefinition(RuntimeDefinition runtimeDefinition,
			IMemento node) {
		node.putString(NAME, runtimeDefinition.getName());
		node.putString(VERSION, runtimeDefinition.getVersion());
		node.putString(TYPE, runtimeDefinition.getType());
		node.putString(LOCATION, runtimeDefinition.getLocation().getAbsolutePath());
		node.putString(DESCRIPTION, runtimeDefinition.getDescription());
		node.putBoolean(ENABLED, runtimeDefinition.isEnabled());
	}

	public static Set<RuntimePath> loadRuntimePathsFromPreferenceString(String preferenceString) {
		HashSet<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
		if (preferenceString == null || preferenceString.isEmpty()) {
			return runtimePaths;
		}
		InputStream is = new BufferedInputStream(new ByteArrayInputStream(preferenceString.getBytes()));
		XMLMemento memento = XMLMemento.createReadRoot(is);
		
		// If there's no preference version declared, it's version 1, 
		// which requires computing the nested runtimes, since
		// they are not stored in the model
		String preferencesVersion = memento.getString(PREFERENCES_VERSION);
		boolean computeIncluded = preferencesVersion == null;
		
		
		IMemento[] nodes = memento.getChildren(RUNTIME_PATH);
		for (IMemento node:nodes) {
			String path = node.getString(PATH);
			boolean scanOnEveryStartup = node.getBoolean(SCAN_ON_EVERY_STAERTUP);
			String tsString = node.getString(TIMESTAMP);
			Long timestamp = null;
			try {
				timestamp = new Long(tsString);
			} catch (NumberFormatException e) {
				// ignore
			}
			RuntimePath runtimePath = new RuntimePath(path);
			runtimePath.setScanOnEveryStartup(scanOnEveryStartup);
			if (timestamp != null) {
				runtimePath.setTimestamp(timestamp);
			}
			IMemento serverDefinitionsNode = node.getChild(SERVER_DEFINITIONS);
			IMemento[] sdNodes = serverDefinitionsNode.getChildren(SERVER_DEFINITION);
			for (IMemento sdNode:sdNodes) {
				RuntimeDefinition runtimeDefinition = createRuntimeDefinition(sdNode);
				runtimeDefinition.setRuntimePath(runtimePath);
				IMemento includedDefinition = sdNode.getChild(INCLUDED_DEFINITION);
				if (includedDefinition != null) {
					IMemento[] includedNodes = includedDefinition
							.getChildren(SERVER_DEFINITION);
					for (IMemento includedNode : includedNodes) {
						RuntimeDefinition included = createRuntimeDefinition(includedNode);
						included.setRuntimePath(runtimePath);
						included.setParent(runtimeDefinition);
						runtimeDefinition.getIncludedRuntimeDefinitions().add(
								included);
					}
				}
				runtimePath.addRuntimeDefinition(runtimeDefinition);
			}
			runtimePaths.add(runtimePath);
		}
		if (computeIncluded) {
			for(RuntimeDefinition definition:getAllRuntimeDefinitions(runtimePaths)) {
				Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
				for (IRuntimeDetector detector:detectors) {
					detector.computeIncludedRuntimeDefinition(definition);
				}
			}
		}
		return runtimePaths;
	}
	
	private static RuntimeDefinition createRuntimeDefinition(IMemento node) {
		String name = node.getString(NAME);
		String version = node.getString(VERSION);
		String type = node.getString(TYPE);
		String location = node.getString(LOCATION);
		String description = node.getString(DESCRIPTION);
		boolean enabled = node.getBoolean(ENABLED);
		RuntimeDefinition runtimeDefinition = 
			new RuntimeDefinition(name, version, type, new File(location));
		runtimeDefinition.setDescription(description);
		runtimeDefinition.setEnabled(enabled);
		return runtimeDefinition;
	}
	
	private static List<RuntimeDefinition> getAllRuntimeDefinitions(Set<RuntimePath> paths) {
		List<RuntimeDefinition> defs = new ArrayList<RuntimeDefinition>();
		for( Iterator<RuntimePath> i = paths.iterator(); i.hasNext(); ) {
			defs.addAll(Arrays.asList(i.next().getRuntimeDefinitions()));
		}
		return defs;
	}
}
