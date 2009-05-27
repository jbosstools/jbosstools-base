package org.jboss.tools.common.gef;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.gef.messages"; //$NON-NLS-1$
	public static String DefaultPaletteCustomizer_ErrorMessage;
	public static String DiagramCopyAction_AcceleratorText;
	public static String DiagramCutAction_AcceleratorText;
	public static String DiagramPasteAction_AcceleratorText;
	public static String PrintRetargetAction_Name;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
