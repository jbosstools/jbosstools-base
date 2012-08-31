package org.jboss.tools.runtime.core;

import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.InvalidRuntimeDetector;

public class RuntimeExtensionManager {
	private static final String RUNTIME_DETECTOR_EXTENSION_ID = "org.jboss.tools.runtime.core.runtimeDetectors";
	
	// Extension point property keys
	private static final String NAME = "name";
	private static final String PREFERENCE_ID = "preferenceId";
	private static final String ID = "id";
	private static final String ENABLED = "enabled";
	private static final String PRIORITY = "priority";

	private static RuntimeExtensionManager manager = null;
	public static RuntimeExtensionManager getDefault() {
		if( manager == null )
			manager = new RuntimeExtensionManager();
		return manager;
	}
	
	// This method will do a full load and actually instantiate the classes
	public Set<IRuntimeDetector> loadDeclaredRuntimeDetectors() {
		Set<IRuntimeDetector> declared = new TreeSet<IRuntimeDetector>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry
				.getExtensionPoint(RUNTIME_DETECTOR_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IRuntimeDetector dec = loadOneDeclaredRuntimeDetector(configurationElements[j]); 
				if( !declared.contains(dec)) {
					declared.add(dec);
				}
			}
		}
		return declared;
	}
	
	// This method will load one detector from a configuration element
	private IRuntimeDetector loadOneDeclaredRuntimeDetector(IConfigurationElement configurationElement) {
		IRuntimeDetector detector;
		try {
			detector = (IRuntimeDetector) configurationElement.createExecutableExtension("class");
		} catch (CoreException e) {
			RuntimeCoreActivator.log(e);
			detector = new InvalidRuntimeDetector();
			detector.setValid(false);
		}
		String name = configurationElement.getAttribute(NAME);
		String preferenceId = configurationElement.getAttribute(PREFERENCE_ID);
		String id = configurationElement.getAttribute(ID);
		detector.setName(name);
		detector.setPreferenceId(preferenceId);
		detector.setId(id);
		String enabled = configurationElement.getAttribute(ENABLED);
		if (enabled == null || new Boolean(enabled).booleanValue()) {
			detector.setEnabled(true);
		} else {
			detector.setEnabled(false);
		}
		String priorityString = configurationElement
				.getAttribute(PRIORITY);
		int priority;
		try {
			priority = Integer.parseInt(priorityString);
		} catch (Exception ex) {
			priority = Integer.MAX_VALUE;
		}
		detector.setPriority(priority);
		return detector;
	}
}
