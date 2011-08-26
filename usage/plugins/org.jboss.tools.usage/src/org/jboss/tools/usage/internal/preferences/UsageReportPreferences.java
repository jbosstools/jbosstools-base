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
package org.jboss.tools.usage.internal.preferences;

import java.io.IOException;

import org.eclipse.jface.preference.IPreferenceStore;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.reporting.ReportingMessages;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;
import org.osgi.service.prefs.BackingStoreException;

// TODO: Auto-generated Javadoc
/**
 * The Class UsageReportPreferences.
 * 
 * @author Andre Dietisheim
 */
public class UsageReportPreferences {

	/**
	 * Enables (<code>true</code>) or disables (<code>false</code>) usage
	 * reporting preferences.
	 * 
	 * @param enabled
	 *            the new enabled
	 */
	public static void setEnabled(boolean enabled) {
		UsageReportPreferencesUtils.getStore().putValue(
					IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID, String.valueOf(enabled));
		save();
	}

	/**
	 * Returns <code>true</code> if the usage reporting enablement is set in the
	 * preferences. Returns <code>false</code> otherwise.
	 * 
	 * @return
	 */
	public static boolean isEnablementSet() {
		String defaultValue = "undefinedValue";
		String value = UsageReportPreferencesUtils.getPreferences().get(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID, defaultValue);
		return value != defaultValue;
	}

	/**
	 * Returns <code>true</code> if usage reporting is enabled
	 * 
	 * @return true, if is enabled
	 */
	public static boolean isEnabled() {
		return UsageReportPreferencesUtils.getPreferences().getBoolean(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID,
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_DEFAULTVALUE);
	}

	/**
	 * Checks if is ask user.
	 * 
	 * @return true, if is ask user
	 */
	public static boolean isAskUser() {
		return UsageReportPreferencesUtils.getPreferences().getBoolean(
				IUsageReportPreferenceConstants.ASK_USER_USAGEREPORT_ID,
				IUsageReportPreferenceConstants.ASK_USER_USAGEREPORT_DEFAULTVALUE);
	}

	/**
	 * Sets the ask user.
	 * 
	 * @param askUser
	 *            the new ask user
	 */
	public static void setAskUser(boolean askUser) {
		UsageReportPreferencesUtils.getStore().putValue(IUsageReportPreferenceConstants.ASK_USER_USAGEREPORT_ID,
				String.valueOf(askUser));
		save();
	}

	/**
	 * Save.
	 */
	private static void save() {
		try {
			UsageReportPreferencesUtils.getStore().save();
		} catch (IOException e) {
			new UsagePluginLogger(JBossToolsUsageActivator.getDefault()).error(
							ReportingMessages.UsageReport_Error_SavePreferences);
		}

	}

	/**
	 * Flush.
	 * 
	 * @throws BackingStoreException
	 *             the backing store exception
	 */
	public static void flush() throws BackingStoreException {
		UsageReportPreferencesUtils.getPreferences().flush();
	}

	/**
	 * Creates the preference store.
	 * 
	 * @return the i preference store
	 */
	public static IPreferenceStore createPreferenceStore() {
		return UsageReportPreferencesUtils.getStore();
	}
}
