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
package org.jboss.tools.common.kb.configuration;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.preference.IPreferenceStore;

import org.jboss.tools.common.kb.KbPlugin;
import org.jboss.tools.common.kb.KbPreferencesConstants;

/**
 * @author igels
 */
public class KbPluginConfiguration implements KbConfiguration {

	public static final String CUSTOM_SCHEMA_LOCATION = copyKbSchemasFromPluginToWorkSpace();

	public static final String CUSTOM_SCHEMA_DTD_LOCATION = CUSTOM_SCHEMA_LOCATION + File.separator + SCHEMA_DTD_FOLDER_NAME;
	public static final String CUSTOM_SCHEMA_TLD_LOCATION = CUSTOM_SCHEMA_LOCATION + File.separator + "custom" + File.separator + SCHEMA_TLD_FOLDER_NAME;
	public static final String SCHEMA_HTML_FILE_LOCATION = KbPlugin.getDefault().getLocation() + File.separator + SCHEMA_FOLDER_NAME + File.separator + SCHEMA_HTML_FOLDER_NAME + File.separator + SCHEMA_HTML_FILE_NAME;
	public static final String SCHEMA_JSP_FILE_LOCATION = KbPlugin.getDefault().getLocation() + File.separator + SCHEMA_FOLDER_NAME + File.separator + SCHEMA_JSP_FOLDER_NAME + File.separator + SCHEMA_JSP_FILE_NAME;
	public static final String CUSTOM_SCHEMA_JSP_DIRECTIVE_FILE_LOCATION = CUSTOM_SCHEMA_LOCATION + File.separator + SCHEMA_JSP_FOLDER_NAME + File.separator + SCHEMA_JSP_DIRECTIVE_FILE_NAME;
	public static final String HTML_MAP_FILE_LOCATION = CUSTOM_SCHEMA_LOCATION + File.separator + SCHEMA_HTML_FOLDER_NAME + File.separator + HTML_MAP_FILE_NAME;

	private static final KbPluginConfiguration INSTANCE = new KbPluginConfiguration();

	private KbPluginConfiguration() {
	}

	private static String copyKbSchemasFromPluginToWorkSpace() {
		IPath path = Platform.getLocation().append(".metadata/.plugins/").append(KbPlugin.PLUGIN_ID).append(SCHEMA_FOLDER_NAME);
		File workSpaceCustomSchemaLocation = path.toFile();

	    return workSpaceCustomSchemaLocation.toString();
	}


	/**
	 * @see org.jboss.tools.common.kb.KbConfiguration#getSchemaPath()
	 */
	public String getDtdSchemaPath() {
		return CUSTOM_SCHEMA_DTD_LOCATION;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#getTldCustomSchemaPath()
	 */
	public String getTldCustomSchemaPath() {
		return CUSTOM_SCHEMA_TLD_LOCATION;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#getHtmlSchemaFilePath()
	 */
	public String getHtmlSchemaFilePath() {
		return SCHEMA_HTML_FILE_LOCATION;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#getHtmlMapFilePath()
	 */
	public String getHtmlMapFilePath() {
		return HTML_MAP_FILE_LOCATION;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#getJspSchemaFilePath()
	 */
	public String getJspSchemaFilePath() {
		return SCHEMA_JSP_FILE_LOCATION;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#getJspDirectiveSchemaFilePath()
	 */
	public String getJspDirectiveSchemaFilePath() {
		return CUSTOM_SCHEMA_JSP_DIRECTIVE_FILE_LOCATION;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#isAllowDownload()
	 */
	public boolean isAllowDownload() {
		IPreferenceStore store = KbPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(KbPreferencesConstants.PROMPTING_ALLOW_DOWNLOAD);
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#isLowerCase()
	 */
	public boolean isLowerCase() {
		IPreferenceStore store = KbPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(KbPreferencesConstants.PROMPTING_USE_LOWER_CASE);
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#isAutocompleteRequiredAttributes()
	 */
	public boolean isAutocompleteRequiredAttributes() {
		IPreferenceStore store = KbPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(KbPreferencesConstants.PROMPTING_USE_AUTOCOMPLETE_FOR_MANDATORY);
	}

	/**
	 * 
	 * @return
	 */
	public static KbPluginConfiguration getInstance() {
		return INSTANCE;
	}

	/**
	 * @see org.jboss.tools.common.kb.configuration.KbConfiguration#isUtilizeComments()
	 */
	public boolean isUtilizeComments() {
		IPreferenceStore store = KbPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(KbPreferencesConstants.PROMPTING_UTILIZE_COMMENTS);
	}
}