/*******************************************************************************
 * Copyright (c) 2013 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.plugin;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.foundation.plugin.log.IPluginLog;
import org.jboss.tools.foundation.plugin.log.PluginLog;
import org.jboss.tools.foundation.plugin.log.StatusFactory;

/**
 * Provides an easy way to log status of events, 
 * or to simply generate status objects for your own use
 * 
 * NOTE: It is useful for bundle activators to have a static method
 * which can return this object.
 * 
 *  For example, in your plugin activator, which 
 *  I will assume is a singleton with a getDefault() accessor,
 *  can have the following:
 *  
 *    public static IPluginLog pluginLog() {
 *    		return (IPluginLog)getDefault().getPluginLog();
 *    }
 *    public static StatusFactory statusFactory() {
 *    	return getDefault().getStatusFactory();
 *    }
 *    
 *   In this way, your non-activator code can then perform:
 *       MyActivator.pluginLog().logError(etc...)
 *       MyActivator.statusFactory().errorStatus(etc...)
 */

public class BaseCorePlugin extends Plugin {

	private IPluginLog pluginLog = null;
	private StatusFactory statusFactory = null;
	
	protected IPluginLog pluginLogInternal() {
		if( pluginLog == null )
			pluginLog = new PluginLog(this);
		return pluginLog;
	}
	
	/**
	 * Get a status factory for this plugin
	 * @return status factory
	 */
	protected StatusFactory statusFactoryInternal() {
		if( statusFactory == null ) 
			statusFactory = new StatusFactory(getBundle().getSymbolicName());
		return statusFactory;
	}

}