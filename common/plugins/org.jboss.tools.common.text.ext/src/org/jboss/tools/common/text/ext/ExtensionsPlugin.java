/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPluginDescriptor;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkDetector;


/**
 * @author Jeremy
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class ExtensionsPlugin extends BaseUIPlugin implements IAdaptable {
	//The shared instance.
	private static ExtensionsPlugin plugin;
	//Resource bundle.
	private ResourceBundle resourceBundle;
	
	public static final String PLUGIN_ID = "org.jboss.tools.common.text.ext"; 

	/**
	 * The constructor.
	 */
    public ExtensionsPlugin() {
        super();
		plugin = this;
		try {
			resourceBundle= ResourceBundle.getBundle("org.jboss.tools.common.text.ext.ExtensionsPlugin");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
    }

    /**
	 * The constructor.
	 */
    public ExtensionsPlugin(IPluginDescriptor descriptor) {
        super();
		plugin = this;
		try {
			resourceBundle= ResourceBundle.getBundle("org.jboss.tools.common.text.ext.ExtensionsPlugin");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
    }

    /**
	 * Returns the shared instance.
	 */
	public static ExtensionsPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the workspace instance.
	 */
	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle= ExtensionsPlugin.getDefault().getResourceBundle();
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	

	public Object getAdapter(Class adapter) {
		if (adapter == IHyperlinkDetector.class) {
			return HyperlinkDetector.getInstance();
		}
		return null;
	}

	
	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}

}
