package org.jboss.tools.common.resref.ui;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.resref.ui.messages"; //$NON-NLS-1$
	public static String AbstractResourceReferencesComposite_Add;
	public static String AbstractResourceReferencesComposite_Edit;
	public static String AbstractResourceReferencesComposite_Remove;
	public static String ResourceReferencesTableProvider_CSSFilePath;
	public static String ResourceReferencesTableProvider_ELExpression;
	public static String ResourceReferencesTableProvider_ImageFolderPath;
	public static String ResourceReferencesTableProvider_Prefix;
	public static String ResourceReferencesTableProvider_Scope;
	public static String ResourceReferencesTableProvider_URI;
	public static String ResourceReferencesTableProvider_Value;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
