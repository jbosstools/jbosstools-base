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
package org.jboss.tools.usage.tracker.internal;

import org.eclipse.osgi.util.NLS;

public class TrackerMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.usage.tracker.internal.messages"; //$NON-NLS-1$

	public static String Tracker_Synchronous;
	public static String Tracker_Asynchronous;
	public static String Tracker_Error;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, TrackerMessages.class);
	}

	private TrackerMessages() {
	}
}
