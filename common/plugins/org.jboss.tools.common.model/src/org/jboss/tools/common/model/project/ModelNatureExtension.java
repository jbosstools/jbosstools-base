package org.jboss.tools.common.model.project;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.jst.web.tld.IFilePathEncoder;

public class ModelNatureExtension {
	public static String EXTENSION_POINT = "org.jboss.tools.common.model.modelnatures"; //$NON-NLS-1$

	String name;
	String displayName;
	String pathEncoder;
	String watcherContributor;
	IFilePathEncoder pathEncoderInstance;

	public ModelNatureExtension() {}

	public String getName() {
		return name;
	}

	public String getDisplayName() {
		return displayName;
	}

	public IFilePathEncoder getPathEncoder () {
		if(pathEncoderInstance != null) {
			return pathEncoderInstance;
		}
		if(pathEncoder == null) {
			return null;
		}
		if(pathEncoder.length() == 0) {
			pathEncoder = null;
			return null;
		}
		try {
			pathEncoderInstance = (IFilePathEncoder)ModelFeatureFactory.getInstance().createFeatureInstance(pathEncoder);
		} catch (ClassCastException e) {
			ModelPlugin.getPluginLog().logError(e);
		} finally {
			pathEncoder = null;
		}
		return pathEncoderInstance;
	}

	public String getWatcherContributor() {
		return watcherContributor;
	}

	static ModelNatureExtension[] INSTANCES;

	public static ModelNatureExtension[] getInstances() {
		if(INSTANCES != null) return INSTANCES;
		List<ModelNatureExtension> list = new ArrayList<ModelNatureExtension>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		IConfigurationElement[] es = point.getConfigurationElements();
		for (IConfigurationElement e: es) {
			ModelNatureExtension n = new ModelNatureExtension();
			n.name = e.getAttribute("name"); //$NON-NLS-1$
			n.displayName = e.getAttribute("displayName"); //$NON-NLS-1$
			n.pathEncoder = e.getAttribute("pathEncoder"); //$NON-NLS-1$
			n.watcherContributor = e.getAttribute("watcherContributor"); //$NON-NLS-1$
			list.add(n);
		}
		return INSTANCES = list.toArray(new ModelNatureExtension[0]);
	}
}
