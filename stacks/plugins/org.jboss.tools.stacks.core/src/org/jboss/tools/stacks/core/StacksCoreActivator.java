package org.jboss.tools.stacks.core;

import org.jboss.tools.foundation.plugin.BaseCorePlugin;
import org.jboss.tools.foundation.plugin.log.IPluginLog;
import org.jboss.tools.foundation.plugin.log.StatusFactory;
import org.osgi.framework.BundleContext;

public class StacksCoreActivator extends BaseCorePlugin {

	public static final String PLUGIN_ID = "org.jboss.tools.stacks.core";
	private static BundleContext context;
	private static StacksCoreActivator DEFAULT;
	
	public static StacksCoreActivator getDefault() {
		return DEFAULT;
	}
	
	public static BundleContext getBundleContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		StacksCoreActivator.context = bundleContext;
		DEFAULT = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		StacksCoreActivator.context = null;
	}
	
	/**
	 * Get the IPluginLog for this plugin. This method 
	 * helps to make logging easier, for example:
	 * 
	 *     FoundationCorePlugin.pluginLog().logError(etc)
	 *  
	 * @return IPluginLog object
	 */
	public static IPluginLog pluginLog() {
		return getDefault().pluginLogInternal();
	}

	/**
	 * Get a status factory for this plugin
	 * @return status factory
	 */
	public static StatusFactory statusFactory() {
		return getDefault().statusFactoryInternal();
	}
	
}
