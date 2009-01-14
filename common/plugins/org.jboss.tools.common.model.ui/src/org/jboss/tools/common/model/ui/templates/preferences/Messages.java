package org.jboss.tools.common.model.ui.templates.preferences;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.templates.preferences.messages"; //$NON-NLS-1$
	public static String ClassTemplateComponent_ADD;
	public static String ClassTemplateComponent_COLUMN_INTERFACES;
	public static String ClassTemplateComponent_EDIT;
	public static String ClassTemplateComponent_LABEL_BASECLASS;
	public static String ClassTemplateComponent_LABEL_INTERFACES;
	public static String ClassTemplateComponent_REMOVE;
	public static String ClassTemplateComponent_XPATH;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
