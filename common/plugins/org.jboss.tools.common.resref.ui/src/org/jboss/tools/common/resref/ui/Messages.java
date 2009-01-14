package org.jboss.tools.common.resref.ui;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.resref.ui.messages"; //$NON-NLS-1$
	public static String AbstractResourceReferencesComposite_Add;
	public static String AbstractResourceReferencesComposite_Edit;
	public static String AbstractResourceReferencesComposite_Remove;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
