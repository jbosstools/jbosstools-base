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
package org.jboss.tools.usage.preferences;

import java.util.Map;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.http.HttpRemotePropertiesProvider;

/**
 * A class that implements a global reporting enablement setting. The current
 * implementation queries a given url and extracts the enablement value out of
 * the response.
 * 
 * @author Andre Dietisheim
 */
public class GlobalUsageSettings {

	private static final String REMOTEPROPS_URL = "http://anonsvn.jboss.org/repos/jbosstools/workspace/usage/usage.properties"; //$NON-NLS-1$

	public static final String REMOTEPROPS_ALLINSTANCES_ENABLED_KEY = "usage_allinstances_enabled="; //$NON-NLS-1$

	private static final boolean REMOTEPROPS_ALLINSTANCES_ENABLED_DEFAULT = false;

	/** the delimiter that delimits the key/value-pairs */
	private static final char VALUE_DELIMITER = '\n';

	/**
	 * system property that enables/disables reporting for current eclipse
	 * instance
	 */
	public static final String SYSPROPS_INSTANCE_ENABLED_KEY = "usage_instance_enabled"; //$NON-NLS-1$

	private static final boolean SYSPROPS_INSTANCE_ENABLED_KEY_DEFAULT = true;

	private HttpRemotePropertiesProvider remoteMap;

	public GlobalUsageSettings(Plugin plugin) {
		remoteMap = createRemoteMap(
				REMOTEPROPS_URL
				, VALUE_DELIMITER
				, plugin
				, REMOTEPROPS_ALLINSTANCES_ENABLED_KEY);
	}

	/**
	 * Returns <code>true</code> if reporting is enabled for all instances
	 * 
	 * @return <code>true, if the remote peer is set to enabled
	 * 
	 * @see #REMOTEPROPS_URL
	 * @see #REMOTEPROPS_ALLINSTANCES_ENABLED_KEY
	 */
	public boolean isAllInstancesReportingEnabled() {
		try {
			Map<String, String> valueMap = remoteMap.getValueMap();
			String isEnabled = valueMap.get(REMOTEPROPS_ALLINSTANCES_ENABLED_KEY);
			return Boolean.valueOf(isEnabled);
		} catch (Exception e) {
			return REMOTEPROPS_ALLINSTANCES_ENABLED_DEFAULT;
		}
	}

	public boolean isInstanceReportingEnabled() {
		return Boolean.valueOf(
				System.getProperty(SYSPROPS_INSTANCE_ENABLED_KEY,
						String.valueOf(SYSPROPS_INSTANCE_ENABLED_KEY_DEFAULT)));
	}

	protected HttpRemotePropertiesProvider createRemoteMap(String url, char valueDelimiter, Plugin plugin,
			String... keys) {
		return new HttpRemotePropertiesProvider(
				url,
				valueDelimiter,
				plugin,
				keys);
	}
}
