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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Map;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.HttpResourceMap;

/**
 * A class that implements a global reporting enablement setting. The current
 * implementation queries a given url and extracts the enablement value out of
 * the response.
 */
public class GlobalUsageReportingSettings extends HttpResourceMap {

	private static final String REPORTING_ENABLEMENT_URL = "http://community.jboss.org/wiki/JBossToolsJBossDeveloperStudioUsageReportingEnablement"; //$NON-NLS-1$

	public static final String KEY_REPORT_ENABLEMENT = "Usage Reporting is ";
	public static final String KEY_DUMMY_VALUE = "Dummy Value is ";
	public static final String KEY_INTEGER_VALUE = "Integer Value is ";

	private static final char VALUE_DELIMITER = '<';


	public GlobalUsageReportingSettings(Plugin plugin) throws IOException {
		super(REPORTING_ENABLEMENT_URL
				, VALUE_DELIMITER
				, plugin
				, KEY_REPORT_ENABLEMENT
				, KEY_DUMMY_VALUE
				, KEY_INTEGER_VALUE);

		this.plugin = plugin;
		this.urlConnection = createURLConnection(REPORTING_ENABLEMENT_URL);
	}

	public boolean isEnabled() throws UnsupportedEncodingException, IOException {
		Map<String, String> valueMap = getValueMap();
		String isEnabled = valueMap.get(KEY_REPORT_ENABLEMENT);
		return isEnabled != null && "ENABLED".equals(isEnabled.toUpperCase());
	}

	public String getStringValue() throws UnsupportedEncodingException, IOException {
		Map<String, String> valueMap = getValueMap();
		return valueMap.get(KEY_DUMMY_VALUE);
	}

	public Integer getIntegerValue() throws UnsupportedEncodingException, IOException {
		Map<String, String> valueMap = getValueMap();
		String integerValue = valueMap.get(KEY_INTEGER_VALUE);
		if(integerValue != null) {
			return Integer.parseInt(integerValue);
		} else {
			return null;
		}
	}
}
