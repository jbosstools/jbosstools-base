package org.jboss.tools.common.verification.ui.vrules.preferences;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.verification.ui.vrules.preferences.messages"; //$NON-NLS-1$
	public static String RulesConfigurationPage_RulesConfigurationTitle;
	public static String VerificationPreferencePage_OptionsTitle;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
