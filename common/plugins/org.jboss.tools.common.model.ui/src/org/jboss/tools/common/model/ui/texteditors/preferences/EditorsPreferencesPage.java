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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.options.Preference;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.preferences.XMOBasedPreferencesPage;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.text.xml.ui.xpl.BasePreferenceConstants;

public class EditorsPreferencesPage extends XMOBasedPreferencesPage {
	
	public static final String EDITOR_PREFERENCES_ID = "org.jboss.tools.common.xstudio.editors"; //$NON-NLS-1$
	
	static String PATH = Preference.EDITOR_PATH;

	public EditorsPreferencesPage() {
		super(_getPreferenceModel().getByPath(PATH));
	}
	
	private static XModel _getPreferenceModel() {
		XModel model = ModelUtilities.getPreferenceModel();
		initialize();
		return model;
	}

	protected static IPreferenceStore store;

	
	// FIXME: Remove Reflection
	private static void initialize() {
		if(store != null) return;
		AbstractUIPlugin plugin = (AbstractUIPlugin)Platform.getPlugin("org.jboss.tools.common.text.xml"); //$NON-NLS-1$
		try {
			Method m = AbstractUIPlugin.class.getDeclaredMethod("initializeDefaultPluginPreferences", new Class[0]); //$NON-NLS-1$
			m.setAccessible(true);
			m.invoke(plugin, new Object[0]);
		} catch(NoSuchMethodException e1) {
			ignore();
		}  catch(IllegalAccessException e2) {
			ignore();
		}  catch(InvocationTargetException e3) {
			ModelUIPlugin.getPluginLog().logError(e3);
		}
		store = (plugin == null) ? null : plugin.getPreferenceStore();
		store.setDefault(BasePreferenceConstants.EDITOR_REPLACE_TAB_WITH_WHITESPACE, false);
	}
	
	static void ignore() {
		//do nothing
	}

}
