package org.jboss.tools.tests;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import junit.framework.TestCase;

public class AbstractRuntimeTest extends TestCase{
	
	public static final String BUNDLE_GROUP_PROVIDER_NAME = "Update Manager Configurator";
	
	/**
	 * @param featureId
	 */
	public Bundle getFirstBundleFor(String featureId) {
		IBundleGroupProvider[] providers = Platform.getBundleGroupProviders();
		System.out.println(providers.length);
		IBundleGroup iBundleGroup = getFirstBundleGroupFor(featureId);
		Bundle[] bundles = iBundleGroup.getBundles();
		for (Bundle bundle : bundles) {
			return bundle;
		}
		return null;
	}

	
	public IBundleGroup getFirstBundleGroupFor(String featureId) {
		IBundleGroupProvider[] providers = Platform.getBundleGroupProviders();
		System.out.println(providers.length);
		for (IBundleGroupProvider iBundleGroupProvider : providers) {
			System.out.println(iBundleGroupProvider.getName());
			IBundleGroup[] bundleGroups = iBundleGroupProvider.getBundleGroups();
			if(BUNDLE_GROUP_PROVIDER_NAME.equals(iBundleGroupProvider.getName())) {
				for (IBundleGroup iBundleGroup : bundleGroups) {
					if(iBundleGroup.getIdentifier().equals(featureId)) {
						return  iBundleGroup;
					}
				}
			}
		}
		return null;
	}

	
	
	public boolean isPluginResolved(String pluginId) {
		Bundle bundle = Platform.getBundle(pluginId);
		assertNotNull(pluginId + " failed to load.", bundle); //$NON-NLS-1$
		try {
			// this line is needed to to force plug-in loading and to change it state to ACTIVE 
			bundle.loadClass("fake class"); //$NON-NLS-1$
		} catch (ClassNotFoundException e) {
			// It happens always because loaded class doesn't not exist
		}
		return ((bundle.getState() & Bundle.RESOLVED) > 0)
				|| ((bundle.getState() & Bundle.ACTIVE) > 0);
	}
}
