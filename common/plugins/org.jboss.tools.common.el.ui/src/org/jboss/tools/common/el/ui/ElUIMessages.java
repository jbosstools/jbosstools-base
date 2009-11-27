package org.jboss.tools.common.el.ui;

import org.eclipse.osgi.util.NLS;

public class ElUIMessages {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.el.ui.messages"; //$NON-NLS-1$

	static {
		NLS.initializeMessages(BUNDLE_NAME, ElUIMessages.class);
	}
	
	public static String RENAME_METHOD_PARTICIPANT_GETTER_WARNING;
	public static String RENAME_METHOD_PARTICIPANT_SETTER_WARNING;
	public static String RENAME_METHOD_PARTICIPANT_OUT_OF_SYNC_FILE;
	public static String RENAME_METHOD_PARTICIPANT_ERROR_PHANTOM_FILE;
	public static String RENAME_METHOD_PARTICIPANT_ERROR_READ_ONLY_FILE;
	public static String RENAME_METHOD_PARTICIPANT_UPDATE_METHOD_REFERENCES;
	public static String RESOURCE_BUNDLES_RENAME_PARTICIPANT_UPDATE_BUNDLE_REFERENCES;
	
	public static String REFACTOR_CONTRIBUTOR_MAIN_MENU;
	public static String REFACTOR_CONTRIBUTOR_RENAME_EL_VARIABLE;
	public static String EL_REFACTOR_RENAME_HANDLER_ERROR;
	public static String RENAME_EL_VARIABLE_WIZARD_EL_VARIABLE_NAME;
}