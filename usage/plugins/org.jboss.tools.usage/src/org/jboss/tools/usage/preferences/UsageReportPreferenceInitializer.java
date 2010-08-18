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

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;

/**
 * @author Andre Dietisheim
 */
public class UsageReportPreferenceInitializer extends
		AbstractPreferenceInitializer {

	public void initializeDefaultPreferences() {
		// IPreferenceStore store =
		// JBossToolsUsageActivator.getDefault().getPreferenceStore();
		// store.setDefault(PreferenceConstants.P_ALLOW_USAGEREPORT, true);
//		IPreferenceStore store = getPreferencesStore();
//		store.setDefault(UsageReportPreferenceConstants.P_ALLOW_USAGEREPORT, true);
		IEclipsePreferences preferences = new ConfigurationScope().getNode(JBossToolsUsageActivator.PLUGIN_ID);
		preferences.putBoolean(IUsageReportPreferenceConstants.USAGEREPORT_ENABLED, true);
	}

	private IPreferenceStore getPreferencesStore() {
		IPreferenceStore preferenceStore = new ScopedPreferenceStore(
				new ConfigurationScope(),
				JBossToolsUsageActivator.PLUGIN_ID);
		return preferenceStore;
	}
}
