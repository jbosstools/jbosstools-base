package org.jboss.tools.common.model.project;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;

public class ModelNatureExtension {
	public static String EXTENSION_POINT = "org.jboss.tools.common.model.modelnatures"; //$NON-NLS-1$

	String name;
	String displayName;
	String pathEncoder;
	String watcherContributor;

	public ModelNatureExtension() {}

	public ModelNatureExtension(String name, String displayName, String pathEncoder, String watcherContributor) {
		super();
		this.name = name;
		this.displayName = displayName;
		this.pathEncoder = pathEncoder;
		this.watcherContributor = watcherContributor;
	}

	public String getName() {
		return name;
	}

	public String getDisplayName() {
		return displayName;
	}

	public String getPathEncoder () {
		return pathEncoder;
	}

	public String getWatcherContributor() {
		return watcherContributor;
	}

	private static final ModelNatureExtension[] INSTANCES;

	static {
		List<ModelNatureExtension> list = new ArrayList<ModelNatureExtension>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		IConfigurationElement[] es = point.getConfigurationElements();
		for (IConfigurationElement e: es) {
			ModelNatureExtension n = new ModelNatureExtension(
				e.getAttribute("name"), //$NON-NLS-1$
				e.getAttribute("displayName"), //$NON-NLS-1$
				e.getAttribute("pathEncoder"), //$NON-NLS-1$
				e.getAttribute("watcherContributor")); //$NON-NLS-1$
			list.add(n);
		}
		INSTANCES = list.toArray(new ModelNatureExtension[0]);
	}

	public static ModelNatureExtension[] getInstances() {
		return INSTANCES;
	}
}
