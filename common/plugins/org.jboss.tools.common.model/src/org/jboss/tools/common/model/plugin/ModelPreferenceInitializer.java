package org.jboss.tools.common.model.plugin;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;

public class ModelPreferenceInitializer extends AbstractPreferenceInitializer {

	/* (non-Javadoc)
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	@Override
	public void initializeDefaultPreferences() {
		((IScopeContext)DefaultScope.INSTANCE).getNode(ModelPlugin.PLUGIN_ID);
		PreferenceModelUtilities.initDefaultPreferenceModel();		
	}

}
