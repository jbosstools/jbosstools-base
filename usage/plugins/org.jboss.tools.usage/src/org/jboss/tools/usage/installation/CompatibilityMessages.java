/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.installation;

import org.eclipse.osgi.util.NLS;

/**
 * @author Alexey Kazakov
 */
public class CompatibilityMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.usage.installation.messages"; //$NON-NLS-1$

	public static String CompatibilityCheckerDialogWarningMessage;
	public static String CompatibilityCheckerEclipseLogWarningMessage;
	public static String DoNotShowAgain;
	public static String CheckingJob;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, CompatibilityMessages.class);
	}

	private CompatibilityMessages() {
	}
}