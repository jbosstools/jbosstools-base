package org.jboss.tools.runtime.core.util;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimeModel;
import org.jboss.tools.runtime.core.model.RuntimePath;

public class RuntimeModelUtil {
	public static final String LINE_DELIMITER = "\n\r\f";//$NON-NLS-1$
	public static final String IN_LINE_DELIMITER = ",";//$NON-NLS-1$
	
	/**
	 * This class will read a properties file of the format: 
	 *   pathName=/some/path/to/jboss,true
	 *   
	 *  Where the path is a path to be scanned, and the 
	 *  following boolean is whether this should be scanned 
	 *  on every startup. 
	 *  
	 * @param file
	 * @return
	 */
	public static Set<RuntimePath> parseRuntimeFile(File file) {
		try {
			return parseRuntimeFile(new BufferedInputStream(new FileInputStream(file)), false);
		} catch(IOException ioe) {
			RuntimeCoreActivator.getDefault().logError(ioe);
		}
		return new TreeSet<RuntimePath>();
	}
	
	/**
	 * This class will read an input stream of the format: 
	 *   pathName=/some/path/to/jboss,true
	 *   
	 *  Where the path is a path to be scanned, and the 
	 *  following boolean is whether this should be scanned 
	 *  on every startup. 
	 *  
	 * @param file
	 * @return
	 */
	public static Set<RuntimePath> parseRuntimeFile(InputStream is, boolean includeMissing) {
		final Set<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
		try {
			Properties servers = new Properties();
			servers.load(is);
				Enumeration<Object> elements = servers.elements();
				while (elements.hasMoreElements()) {
					String str = (String) elements.nextElement();
				StringTokenizer lineTokenizer = 
						new StringTokenizer(str,LINE_DELIMITER); 
				while (lineTokenizer.hasMoreTokens()) {
					String lineToken = lineTokenizer.nextToken();
					StringTokenizer tokenizer = new StringTokenizer(
							lineToken, IN_LINE_DELIMITER);
					if (tokenizer.countTokens() == 2) {
						String location = tokenizer.nextToken();
						boolean scan = Boolean.parseBoolean(tokenizer.nextToken());
						File locationFile = new File(location);
						if (locationFile.isDirectory() || (includeMissing && !locationFile.exists())) {
							RuntimePath tempLocation = new RuntimePath(location);
							tempLocation.setScanOnEveryStartup(scan);
							runtimePaths.add(tempLocation);
						}
					}
				}
			}
		} catch (IOException e) {
			RuntimeCoreActivator.getDefault().logError(e);
		}
		return runtimePaths;
	}

	public static void updateTimestamps(RuntimePath[] runtimePaths2) {
		for (RuntimePath runtimePath : runtimePaths2) {
			String path = runtimePath.getPath();
			if (path != null && !path.isEmpty()) {
				File directory = new File(path);
				if (directory.isDirectory()) {
					runtimePath.setTimestamp(directory.lastModified());
				}
			}
		}
	}


	public static boolean verifyRuntimeDefinitionCreated(RuntimeDefinition runtimeDefinition) {
		return verifyRuntimeDefinitionCreated(runtimeDefinition, true);
	}
	public static boolean verifyRuntimeDefinitionCreated(RuntimeDefinition runtimeDefinition, boolean checkNested) {
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		return verifyRuntimeDefinitionCreated(runtimeDefinition, detectors, checkNested);
	}
	public static boolean verifyRuntimeDefinitionCreated(RuntimeDefinition runtimeDefinition, 
			Set<IRuntimeDetector> detectors, boolean checkNested) {
		boolean created = false;
		for (IRuntimeDetector detector:detectors) {
			if (!detector.isEnabled()) {
				continue;
			}
			if (detector.exists(runtimeDefinition)) {
				if( !checkNested ) {
					created = true;
				} else {
					List<RuntimeDefinition> includedDefinitions = runtimeDefinition.getIncludedRuntimeDefinitions();
					boolean includedCreated = true;
					for (RuntimeDefinition includedDefinition:includedDefinitions) {
						if (!verifyRuntimeDefinitionCreated(includedDefinition)) {
							includedCreated = false;
							break;
						}
					}
					if (includedCreated) {
						created = true;
						break;
					}
				}
			}
		}
		return (created);
	}

	public static List<RuntimeDefinition> getRuntimeDefinitions(Set<RuntimePath> runtimePaths) {
		RuntimePath[] paths = (RuntimePath[]) runtimePaths.toArray(new RuntimePath[runtimePaths.size()]);
		return getRuntimeDefinitions(paths);
	}
	
	public static List<RuntimeDefinition> getRuntimeDefinitions(RuntimePath[] runtimePaths) {
		ArrayList<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		for (RuntimePath runtimePath:runtimePaths) {
			runtimeDefinitions.addAll(Arrays.asList(runtimePath.getRuntimeDefinitions()));
		}
		return runtimeDefinitions;
	}
	
	public static List<RuntimeDefinition> getAllDefinitions(RuntimePath runtimePath) {
		return getAllDefinitions(new RuntimePath[]{runtimePath});
	}

	public static List<RuntimeDefinition> getAllDefinitions(RuntimePath[] runtimePath) {
		List<RuntimeDefinition> allDefinitions = new ArrayList<RuntimeDefinition>();
		for( int i = 0; i < runtimePath.length; i++ ) {
			allDefinitions.addAll(Arrays.asList(runtimePath[i].getRuntimeDefinitions()));
			for (RuntimeDefinition runtimeDefinition : runtimePath[i].getRuntimeDefinitions()) {
				allDefinitions.addAll(runtimeDefinition.getIncludedRuntimeDefinitions());
			}
		}
		return allDefinitions;
	}

	public static boolean runtimeDefinitionsExists(RuntimeDefinition runtimeDefinition, RuntimeModel model) {
		return runtimeDefinitionsExists(runtimeDefinition, model.getRuntimePaths());
	}

	public static boolean runtimeDefinitionsExists(RuntimeDefinition runtimeDefinition, RuntimePath[] paths) {
		return runtimeDefinitionsExists(runtimeDefinition, getRuntimeDefinitions(paths));
	}
	
	public static boolean runtimeDefinitionsExists(RuntimeDefinition runtimeDefinition,
			List<RuntimeDefinition> allRuntimeDefinitions) {
		Iterator<RuntimeDefinition> it = allRuntimeDefinitions.iterator();
		while(it.hasNext()) {
			if( it.next().equals(runtimeDefinition))
				return true;
		}
		return false;
	}

}
