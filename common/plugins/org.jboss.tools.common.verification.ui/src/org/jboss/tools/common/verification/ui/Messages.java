package org.jboss.tools.common.verification.ui;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.verification.ui.messages"; //$NON-NLS-1$
	public static String SignificanceView_Any;
	public static String SignificanceView_GreaterThanN;
	public static String SignificanceView_OnlyTen;
	public static String SignificanceView_VerificationLevelLabel;
	public static String VerifyWizardView_VerifyTask;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
