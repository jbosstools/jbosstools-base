package org.jboss.tools.common.verification;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.verification.messages"; //$NON-NLS-1$
	public static String VerifyRulesAllHandler_ApplyVerificationRules;
	public static String VerifyRulesAllHandler_ApplyVerificationRulesForObject;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
