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
import java.util.Map;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.HttpResourceMap;

/**
 * A class that implements a global reporting enablement setting. The current
 * implementation queries a given url and extracts the enablement value out of
 * the response.
 */
public class GlobalUsageReportingSettings extends HttpResourceMap {

	private static final String REPORT_ENABLEMENT_ENABLEDVALUE = "ENABLED"; //$NON-NLS-1$

	private static final String REPORTING_ENABLEMENT_URL = "http://community.jboss.org/wiki/JBossToolsJBossDeveloperStudioUsageReportingEnablement"; //$NON-NLS-1$

	public static final String REPORT_ENABLEMENT_KEY = "Usage Reporting is "; //$NON-NLS-1$
	private static final boolean REPORT_ENABLEMENT_DEFAULT = true;

	public static final String DUMMY_VALUE_KEY = "Dummy Value is "; //$NON-NLS-1$
	public static final String INTEGER_VALUE_KEY = "Integer Value is "; //$NON-NLS-1$

	/* the delimiter that delimits the value */
	private static final char VALUE_DELIMITER = '<';

	public GlobalUsageReportingSettings(Plugin plugin) throws IOException {
		super(REPORTING_ENABLEMENT_URL
				, VALUE_DELIMITER
				, plugin
				, REPORT_ENABLEMENT_KEY
				, DUMMY_VALUE_KEY
				, INTEGER_VALUE_KEY);

		this.plugin = plugin;
		this.urlConnection = createURLConnection(REPORTING_ENABLEMENT_URL);
	}

	public boolean isEnabled() {
		try {
			Map<String, String> valueMap = getValueMap();
			String isEnabled = valueMap.get(REPORT_ENABLEMENT_KEY);
			return isEnabled != null && REPORT_ENABLEMENT_ENABLEDVALUE.equals(isEnabled.toUpperCase());
		} catch (Exception e) {
			return REPORT_ENABLEMENT_DEFAULT;
		}
	}

	public String getStringValue() {
		try {
			Map<String, String> valueMap = getValueMap();
			return valueMap.get(DUMMY_VALUE_KEY);
		} catch (Exception e) {
			return "";
		}
	}

	public Integer getIntegerValue() {
		Integer returnValue = null;
		try {
			Map<String, String> valueMap = getValueMap();
			String integerValue = valueMap.get(INTEGER_VALUE_KEY);
			if (integerValue != null) {
				returnValue = Integer.parseInt(integerValue);
			}
		} catch (Exception e) {
		}
		return returnValue;
	}
}
