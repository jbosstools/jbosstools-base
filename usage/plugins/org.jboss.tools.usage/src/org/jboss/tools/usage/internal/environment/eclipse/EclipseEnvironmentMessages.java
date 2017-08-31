/*******************************************************************************
 * Copyright (c) 2010-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.environment.eclipse;

import org.eclipse.osgi.util.NLS;

/**
 * @author Andre Dietisheim
 */
public class EclipseEnvironmentMessages extends NLS {
	
	private static final String BUNDLE_NAME = "org.jboss.tools.usage.internal.environment.eclipse.messages"; //$NON-NLS-1$

	public static String EclipseEnvironment_Error_SavePreferences;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, EclipseEnvironmentMessages.class);
	}

	private EclipseEnvironmentMessages() {
	}
}
