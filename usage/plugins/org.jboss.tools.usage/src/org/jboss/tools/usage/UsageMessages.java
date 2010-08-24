/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage;

import org.eclipse.osgi.util.NLS;

public class UsageMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.usage.messages"; //$NON-NLS-1$
	public static String Tracker_Synchronous;
	public static String Tracker_Asynchronous;
	public static String Tracker_Error;

	public static String HttpGetMethod_Error_Http;
	public static String HttpGetMethod_Error_Io;
	public static String HttpGetMethod_Success;

	public static String KillSwitchPreference_Error_Exception;
	public static String KillSwitchPreference_Error_Http;
	public static String KillSwitchPreference_Info_HttpQuery;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, UsageMessages.class);
	}

	private UsageMessages() {
	}
}
