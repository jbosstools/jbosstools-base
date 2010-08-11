package org.jboss.tools.runtime.preferences;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.jboss.tools.runtime.Activator;

public class JBossRuntimePreferencesInitializer extends
		AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		Preferences preferences = Activator.getDefault().getPluginPreferences();
		preferences.setDefault(
				Activator.FIRST_START,
				true);
	}

}
