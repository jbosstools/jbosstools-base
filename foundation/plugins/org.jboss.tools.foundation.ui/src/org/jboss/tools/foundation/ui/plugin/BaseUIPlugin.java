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
package org.jboss.tools.foundation.ui.plugin;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.foundation.core.plugin.log.IPluginLog;
import org.jboss.tools.foundation.core.plugin.log.PluginLog;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;

/**
 * Acts as a base activator superclass which can
 * handle and expose logging and other common tasks that
 * a UI plugin would need.
 */
public class BaseUIPlugin extends AbstractUIPlugin {
	private IPluginLog pluginLog = null;
	private StatusFactory statusFactory = null;
	private BaseUISharedImages sharedImages = null;
	
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
	
	/**
	 * Access the shared images for the plugin
	 * @return
	 */
	public BaseUISharedImages getSharedImages() {
		if( sharedImages == null ) {
			sharedImages = createSharedImages();
			
		}
		return sharedImages;
	}
	
	/**
	 * Create your shared images instance. Clients are expected to override this
	 */
	protected BaseUISharedImages createSharedImages() {
		return new BaseUISharedImages(getBundle());
	}
	
}
