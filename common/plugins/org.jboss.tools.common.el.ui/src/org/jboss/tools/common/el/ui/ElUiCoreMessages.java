package org.jboss.tools.common.el.ui;

import org.eclipse.osgi.util.NLS;

public class ElUiCoreMessages {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.el.ui.messages"; //$NON-NLS-1$

	static {
		NLS.initializeMessages(BUNDLE_NAME, ElUiCoreMessages.class);
	}
	
	public static String RENAME_METHOD_PARTICIPANT_GETTER_WARNING;
	public static String RENAME_METHOD_PARTICIPANT_SETTER_WARNING;
	public static String RENAME_METHOD_PARTICIPANT_OUT_OF_SYNC_FILE;
	public static String RENAME_METHOD_PARTICIPANT_ERROR_PHANTOM_FILE;
	public static String RENAME_METHOD_PARTICIPANT_ERROR_READ_ONLY_FILE;
	public static String RENAME_METHOD_PARTICIPANT_UPDATE_METHOD_REFERENCES;

}