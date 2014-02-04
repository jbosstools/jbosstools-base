/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.core;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.foundation.core.plugin.BaseCorePlugin;
import org.jboss.tools.foundation.core.plugin.log.IPluginLog;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;
import org.jboss.tools.foundation.core.usage.internal.UsageTrackerService;
import org.osgi.framework.BundleContext;

/**
 * This class is here primarily as an example implementation
 * of a proper subclass of BaseCorePlugin. Subclasses of 
 * BaseCorePlugin may need some or all of the methods listed
 * here for access to the most commen resources that 
 * Activators typically expose internally to their own plugin.
 * 
 *  This class is internal and is in a private (not exported) package.
 */
public class FoundationCorePlugin extends BaseCorePlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.foundation.core"; //$NON-NLS-1$
	private static FoundationCorePlugin instance;
	private static BundleContext myContext;
	
	private UsageTrackerService usageTrackerService;
	
	public FoundationCorePlugin() {
		super();
		instance = this;
	}

	public static FoundationCorePlugin getDefault() {
	    return instance;
	}

	public static BundleContext getBundleContext() {
	    return myContext;
	}
	
	@Override
	public void stop(BundleContext context) throws Exception {
		usageTrackerService.close();
		super.stop(context);
	}

    public void start(BundleContext context) throws Exception {
        super.start(context);
        myContext = context;
        initUsageTrackerService(context);
        registerDebugOptionsListener(PLUGIN_ID, new Trace(this), context);
	}
    
    private void initUsageTrackerService(BundleContext context) {
    	usageTrackerService = new UsageTrackerService(getBundle().getBundleContext());
    	usageTrackerService.open();
	}
    
	/**
	 * Gets message from plugin.properties
	 * @param key
	 * @return
	 */
	public static String getMessage(String key)	{
		return Platform.getResourceString(instance.getBundle(), key);
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
	
	public UsageTrackerService getUsageTrackerService() {
		return usageTrackerService;
	}
}
