package org.jboss.tools.common.resref.core;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.resref.core.messages"; //$NON-NLS-1$
	public static String ResourceReference_Folder;
	public static String ResourceReference_Global;
	public static String ResourceReference_Page;
	public static String ResourceReference_Project;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
