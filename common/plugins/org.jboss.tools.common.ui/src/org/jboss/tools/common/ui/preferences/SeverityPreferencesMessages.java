/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.ui.preferences;

import org.eclipse.osgi.util.NLS;

/**
 * @author Viacheslav Kabanovich
 */
public class SeverityPreferencesMessages extends NLS {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.ui.preferences.SeverityPreferencesMessages"; //$NON-NLS-1$

	public static String ValidatorConfigurationBlock_needsbuild_title;
	public static String ValidatorConfigurationBlock_needsfullbuild_message;
	public static String ValidatorConfigurationBlock_needsprojectbuild_message;

	public static String VALIDATOR_CONFIGURATION_BLOCK_ERROR;
	public static String VALIDATOR_CONFIGURATION_BLOCK_IGNORE;
	public static String VALIDATOR_CONFIGURATION_BLOCK_WARNING;

	static {
		NLS.initializeMessages(BUNDLE_NAME, SeverityPreferencesMessages.class);
	}
}