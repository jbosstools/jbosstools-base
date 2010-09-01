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
import org.jboss.tools.usage.HttpResourceMap;

/**
 * A class that implements a global reporting enablement setting. The current
 * implementation queries a given url and extracts the enablement value out of
 * the response.
 */
public class GlobalUsageReportingSettings extends HttpResourceMap {

	private static final String REPORTING_ENABLEMENT_URL = "http://anonsvn.jboss.org/repos/jbosstools/workspace/usage/usage.properties"; //$NON-NLS-1$

	public static final String REPORT_ENABLEMENT_KEY = "usage_reporting_enabled="; //$NON-NLS-1$
	private static final boolean REPORT_ENABLEMENT_DEFAULT = false;

	/* the delimiter that delimits the value */
	private static final char VALUE_DELIMITER = '\n';

	public GlobalUsageReportingSettings(Plugin plugin) {
		super(REPORTING_ENABLEMENT_URL
				, VALUE_DELIMITER
				, plugin
				, REPORT_ENABLEMENT_KEY);

		this.plugin = plugin;
	}

	public boolean isEnabled() {
		try {
			Map<String, String> valueMap = getValueMap();
			String isEnabled = valueMap.get(REPORT_ENABLEMENT_KEY);
			return Boolean.valueOf(isEnabled);
		} catch (Exception e) {
			return REPORT_ENABLEMENT_DEFAULT;
		}
	}
}
