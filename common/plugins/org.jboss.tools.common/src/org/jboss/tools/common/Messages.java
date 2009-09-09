package org.jboss.tools.common;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.messages"; //$NON-NLS-1$
	public static String BaseUIPlugin_ErrorDialogTitle;
	public static String SAXValidator_IOExceptionMessage;
	public static String SAXValidator_SAXExceptionMessage;
	public static String SAXValidator_UnableToInstantiateMessage;
	public static String XMLUtilities_IOExceptionMessage;
	public static String XMLUtilities_SAXExceptionMessage;
	public static String MethodNotImplementedException_MethodIsNotImplementedYet;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
