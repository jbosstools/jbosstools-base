/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
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
