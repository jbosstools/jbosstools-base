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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.internal.ui.IJavaStatusConstants;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import org.jboss.tools.common.model.plugin.IModelPlugin;
import org.jboss.tools.common.reporting.ProblemReporterFactory;
import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.reporting.ProblemReporter;
import org.jboss.tools.common.model.ui.templates.RedHatTemplateContextType;

public class ModelUIPlugin extends AbstractUIPlugin implements IModelPlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.common.model.ui"; 
	private static ModelUIPlugin INSTANCE;
	private ResourceBundle resourceBundle;

	public ModelUIPlugin() {
		INSTANCE = this;
		try {
			resourceBundle= ResourceBundle.getBundle(PLUGIN_ID + ".PluginResources");
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
		configureJavaPlugin();
	}

	private void configureJavaPlugin() {
		TemplateContextType contextType = new RedHatTemplateContextType();
		JavaPlugin.getDefault().getTemplateContextRegistry().addContextType(contextType);
	}
	
	protected void initializeDefaultPluginPreferences() {		
		Platform.getBundle(JavaUI.ID_PLUGIN);		
		PreferenceConstants.initializeDefaultValues(getPreferenceStore());
	}

	public static ModelUIPlugin getDefault() {
		return INSTANCE;
	}

	public static boolean isDebugEnabled() {
		return INSTANCE.isDebugging();
	}
	
	public static void log(String msg) {
		if(isDebugEnabled()) INSTANCE.getLog().log(new Status(Status.INFO, PLUGIN_ID, Status.OK, msg, null));		
	}
	
	public static void log(IStatus status) {
		if(isDebugEnabled() || !status.isOK()) INSTANCE.getLog().log(status);
	}
	
	public static void log(String message, Throwable exception) {
		INSTANCE.getLog().log(new Status(Status.ERROR, PLUGIN_ID, Status.OK, message, exception));		
	}
	
	public static void log(Exception ex) {
		INSTANCE.getLog().log(new Status(Status.ERROR, PLUGIN_ID, Status.OK, "No message", ex));
	}

    public static final String PROJECT_OVERRIDE = "org.jboss.tools.common.model.ui.templates.projectOverriveTemplates";
    public static final String PROJECT_CAN_OVERRIDE = "org.jboss.tools.common.model.ui.templates.projectCanOverriveTemplates";
	public static final String ID_PLUGIN= "org.jboss.tools.common.model.ui.templates";
	public static final String META_TEMPLATES_DTD = "-//Red Hat Inc.//DTD Meta Templates 1.0//EN";

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        store.setDefault(PROJECT_CAN_OVERRIDE, false);
        super.initializeDefaultPreferences(store);
    }

	public static void log(String s, boolean isError) {
		if (isError) {
			log(new Status(IStatus.ERROR, getPluginId(), IJavaStatusConstants.INTERNAL_ERROR, s, null));
		} else {
			log(new Status(IStatus.INFO, getPluginId(), IJavaStatusConstants.INTERNAL_ERROR, s, null));
		}
	}
	
	public static String getPluginId() {
		return ModelUIPlugin.ID_PLUGIN;
	}

}
