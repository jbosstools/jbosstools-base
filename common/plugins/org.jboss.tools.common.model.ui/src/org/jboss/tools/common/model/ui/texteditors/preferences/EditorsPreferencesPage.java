/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.texteditors.preferences;

import java.lang.reflect.Method;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.preferences.XMOBasedPreferencesPage;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.text.xml.ui.xpl.BasePreferenceConstants;

public class EditorsPreferencesPage extends XMOBasedPreferencesPage {
	static String PATH = "%Options%/Struts Studio/Editors";

	public EditorsPreferencesPage() {
		super(_getPreferenceModel().getByPath(PATH));
	}
	
	private static XModel _getPreferenceModel() {
		XModel model = ModelUtilities.getPreferenceModel();
		try {
			initialize();
		} catch (Exception t) {
			ModelUIPlugin.getPluginLog().logError("Error in initializing editors preference page", t);
		}
		return model;
	}

	protected static IPreferenceStore store;

	private static void initialize() {
		if(store != null) return;
		AbstractUIPlugin plugin = (AbstractUIPlugin)Platform.getPlugin("org.jboss.tools.common.text.xml");
		try {
			Method m = AbstractUIPlugin.class.getDeclaredMethod("initializeDefaultPluginPreferences", new Class[0]);
			m.setAccessible(true);
			m.invoke(plugin, new Object[0]);
		} catch (Exception e) {
			//ignore
		}
		store = (plugin == null) ? null : plugin.getPreferenceStore();
		store.setDefault(BasePreferenceConstants.EDITOR_REPLACE_TAB_WITH_WHITESPACE, false);
	}

}
