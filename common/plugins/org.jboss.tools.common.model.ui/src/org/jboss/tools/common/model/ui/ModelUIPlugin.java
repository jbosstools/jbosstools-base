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
package org.jboss.tools.common.model.ui;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.model.plugin.IModelPlugin;
import org.osgi.framework.BundleContext;

public class ModelUIPlugin extends BaseUIPlugin implements IModelPlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.common.model.ui";  //$NON-NLS-1$
	private static ModelUIPlugin INSTANCE;
	private ResourceBundle resourceBundle;

	public ModelUIPlugin() {
		INSTANCE = this;
		try {
			resourceBundle= ResourceBundle.getBundle(PLUGIN_ID + ".PluginResources"); //$NON-NLS-1$
		} 
		catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}

	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	public static String getResourceString(String key) {
		ResourceBundle bundle= ModelUIPlugin.getDefault().getResourceBundle();
		try	{
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	public static ModelUIPlugin getDefault() {
		return INSTANCE;
	}

	public static boolean isDebugEnabled() {
		return INSTANCE.isDebugging();
	}
	

    public static final String PROJECT_OVERRIDE = "org.jboss.tools.common.model.ui.templates.projectOverriveTemplates"; //$NON-NLS-1$
//    public static final String PROJECT_CAN_OVERRIDE = "org.jboss.tools.common.model.ui.templates.projectCanOverriveTemplates";
	public static final String ID_PLUGIN= "org.jboss.tools.common.model.ui.templates"; //$NON-NLS-1$
	public static final String META_TEMPLATES_DTD = "-//Red Hat Inc.//DTD Meta Templates 1.0//EN"; //$NON-NLS-1$

	public static String getPluginId() {
		return ModelUIPlugin.ID_PLUGIN;
	}
	
	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}
}