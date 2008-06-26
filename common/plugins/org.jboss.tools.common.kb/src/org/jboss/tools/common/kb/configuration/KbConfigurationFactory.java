/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.kb.configuration;

/**
 * @author igels
 */
public class KbConfigurationFactory {

	public static final String CONFIGURATION_SYSTEM_PROPERTY_NAME = "org.jboss.tools.common.kb.configuration";

	public static final String TEST_CONFIGURATION = "test";
	public static final String PLUGIN_CONFIGURATION = "plugin";

	private static final KbConfigurationFactory INSTANCE = new KbConfigurationFactory();

	private KbConfigurationFactory() {
	}

	/**
	 * 
	 * @return
	 */
	public static KbConfigurationFactory getInstance() {
		return INSTANCE;
	}

	/**
	 * 
	 * @return
	 */
	public KbConfiguration getPluginConfiguration() {
		return KbPluginConfiguration.getInstance();
	}

	/**
	 * 
	 * @return
	 */
	public KbConfiguration getDefaultConfiguration() {
		return getPluginConfiguration();
	}
}