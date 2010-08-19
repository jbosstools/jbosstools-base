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
package org.jboss.tools.usage.reporting;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.preferences.IUsageReportPreferenceConstants;
import org.jboss.tools.usage.util.PreferencesUtils;
import org.jboss.tools.usage.util.StatusUtils;
import org.osgi.service.prefs.BackingStoreException;

public class UsageReportPreferences {

	public static void setEnabled(boolean enabled) {
		PreferencesUtils.getStore().putValue(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID, String.valueOf(enabled));
	}

	public static boolean isEnabled() {
		return PreferencesUtils.getPreferences().getBoolean(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID,
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_DEFAULTVALUE);
	}

	public static void flush() throws BackingStoreException {
		PreferencesUtils.getPreferences().flush();
	}

	public static IPreferenceStore createPreferenceStore() {
		return PreferencesUtils.getStore();
	}

	public static boolean isAskUser() {
		IEclipsePreferences preferences = PreferencesUtils.getPreferences();
		return preferences.getBoolean(IUsageReportPreferenceConstants.ASK_USER_USAGEREPORT_ID, true);
	}

	public static void setAskUser(boolean askUser) {
		try {
			IEclipsePreferences preferences = PreferencesUtils.getPreferences();
			preferences.putBoolean(IUsageReportPreferenceConstants.ASK_USER_USAGEREPORT_ID, askUser);
			preferences.flush();
		} catch (BackingStoreException e) {
			JBossToolsUsageActivator.getDefault().getLog().log(
					StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID,
							ReportingMessages.UsageReport_Error_SavePreferences, e,
							IUsageReportPreferenceConstants.ASK_USER_USAGEREPORT_ID));
		}
	}
}
