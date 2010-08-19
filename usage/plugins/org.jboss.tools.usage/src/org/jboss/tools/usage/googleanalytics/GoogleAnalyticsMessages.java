package org.jboss.tools.usage.googleanalytics;

import org.eclipse.osgi.util.NLS;

public class GoogleAnalyticsMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.usage.googleanalytics.googleanalytics_messages"; //$NON-NLS-1$
	public static String HttpGetMethod_Error_Http;
	public static String HttpGetMethod_Error_Io;
	public static String HttpGetMethod_Success;
	public static String Tracker_Synchronous;
	public static String Tracker_Asynchronous;
	public static String Tracker_Error;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, GoogleAnalyticsMessages.class);
	}

	private GoogleAnalyticsMessages() {
	}
}
