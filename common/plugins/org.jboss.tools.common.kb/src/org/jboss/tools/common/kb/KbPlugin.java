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
package org.jboss.tools.common.kb;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author eskimo
 */
public class KbPlugin extends AbstractUIPlugin {

	public static final String PLUGIN_ID = "org.jboss.tools.common.kb";
	private File location;

	protected void initializeDefaultPluginPreferences() {
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_USE_LOWER_CASE, true);
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_USE_AUTOCOMPLETE_FOR_MANDATORY, true);
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_UTILIZE_COMMENTS, false);		
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_USE_AUTOCOMPLETE_FOR_TAGS, true);
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_ALLOW_DOWNLOAD, true);
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_USE_PROXY, false);
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_HOST, "");
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_PORT, "");
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_USER, "");
		getPreferenceStore().setDefault(KbPreferencesConstants.PROMPTING_PASSWORD, "");
	}

	/**
	 * 
	 *
	 */
	public KbPlugin() {
	}

	/**
	 * 
	 * @return
	 */
	public static KbPlugin getDefault() {
		return KbPluginHolder.INSTANCE;
	}

	private boolean isLocationSet = false;

	/**
	 * Return plugin Location.
	 * @return
	 */
	public File getLocation() {
		if(!isLocationSet) {
			try {
				isLocationSet = true;
				location = new File(FileLocator.resolve(KbPlugin.getDefault().getBundle().getEntry("/")).getPath());
			} catch (IOException e) {
				ProblemReportingHelper.reportProblem(KbPlugin.PLUGIN_ID, e);
				KbPlugin.log(e);
			}
		}
		return location;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean isDebugEnabled() {
		return getDefault().isDebugging();
	}

	/**
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
    public void start(BundleContext context) throws Exception {
        super.start(context);
        KbConfigurationFactory.getInstance().getPluginConfiguration();
    }

    static class KbPluginHolder {
		static KbPlugin INSTANCE = (KbPlugin)Platform.getPlugin(PLUGIN_ID); 
	}

	public static void log(String msg) {
		getDefault().getLog().log(new Status(Status.INFO, PLUGIN_ID, Status.OK, msg, null));		
	}
	
	public static void log(String message, Throwable exception) {
		getDefault().getLog().log(new Status(Status.ERROR, PLUGIN_ID, Status.OK, message, exception));
	}
	
	static public void log(Exception ex) {
		getDefault().getLog().log(new Status(Status.ERROR, PLUGIN_ID, Status.OK, "No message", ex));
	}
	
}