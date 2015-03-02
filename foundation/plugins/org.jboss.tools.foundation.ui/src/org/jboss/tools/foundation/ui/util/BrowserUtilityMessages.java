package org.jboss.tools.foundation.ui.util;

import org.eclipse.osgi.util.NLS;

public class BrowserUtilityMessages {
	
	private static String BUNDLE_NAME = BrowserUtilityMessages.class.getName();
	
	public static String 
		Copy_URL_to_clipboard,
		Open_in_external_browser,
		Embedded_browser_cannot_be_opened;
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, BrowserUtilityMessages.class);
	}
}
