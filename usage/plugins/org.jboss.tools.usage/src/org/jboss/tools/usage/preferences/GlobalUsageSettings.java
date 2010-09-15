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

	/**
	 * system property that enables/disables reporting for current eclipse
	 * instance
	 */
	public static final String USAGE_REPORTING_ENABLED_KEY = "usage_reporting_enabled"; //$NON-NLS-1$

	public static final String REMOTEPROPS_USAGE_REPORTING_ENABLED_KEY = USAGE_REPORTING_ENABLED_KEY + "="; //$NON-NLS-1$

	private static final boolean USAGE_REPORTING_ENABLED_DEFAULT = true;

	/** the delimiter that delimits the key/value-pairs */
	private static final char VALUE_DELIMITER = '\n';

	private HttpRemotePropertiesProvider remoteMap;

	public GlobalUsageSettings(Plugin plugin) {
		remoteMap = createRemoteMap(
				REMOTEPROPS_URL
				, VALUE_DELIMITER
				, plugin
				, REMOTEPROPS_USAGE_REPORTING_ENABLED_KEY);
	}

	/**
	 * Returns <code>true</code> if usage reporting is turned on.
	 * 
	 * @return true, if is reporting enabled
	 */
	public boolean isReportingEnabled() {
		return isInstanceReportingEnabled() && isAllInstancesReportingEnabled();
	}

	/**
	 * Returns <code>true</code> if reporting is enabled for all instances. The
	 * appropriate setting is queried in a remote properties file at {@link #REMOTEPROPS_URL}. The key is {@link #REMOTEPROPS_ALLINSTANCES_ENABLED_DEFAULT}
	 * 
	 * @return <code>true, if the remote peer is set to enabled
	 * 
	 * @see #REMOTEPROPS_URL
	 * @see #REMOTEPROPS_ALLINSTANCES_ENABLED_KEY
	 */
	private boolean isAllInstancesReportingEnabled() {
		try {
			Map<String, String> valueMap = remoteMap.getValueMap();
			String isEnabled = valueMap.get(REMOTEPROPS_USAGE_REPORTING_ENABLED_KEY);
			if (isEnabled == null) {
				return USAGE_REPORTING_ENABLED_DEFAULT;
			}

			return Boolean.valueOf(isEnabled);
		} catch (Exception e) {
			return USAGE_REPORTING_ENABLED_DEFAULT;
		}
	}

	/**
	 * Returns <code>true</code> if is reporting is enabled for this eclipse
	 * instance.
	 * 
	 * @return true, if this instance shall report usage
	 * 
	 * @see #SYSPROPS_INSTANCE_ENABLED_KEY
	 */
	private boolean isInstanceReportingEnabled() {
		return Boolean.valueOf(
				System.getProperty(USAGE_REPORTING_ENABLED_KEY,
						String.valueOf(USAGE_REPORTING_ENABLED_DEFAULT)));
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
