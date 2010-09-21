/******************************************************************************* 
 * Copyright (c) 2007-2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.ca.preferences;

import java.io.IOException;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.jboss.tools.common.el.core.ELCorePlugin;

/**
 * Initializer for EL Content Assistant preferences
 * 
 * @author jeremy
 *
 */
public class ELContentAssistPreferenceInitializer extends AbstractPreferenceInitializer {

	public ELContentAssistPreferenceInitializer() {}

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = ELCorePlugin.getDefault().getPreferenceStore();
		store.setDefault(ELContentAssistPreferences.SHOW_GETTERS_AND_SETTERS, false);
		store.setDefault(ELContentAssistPreferences.SHOW_METHODS_WITH_PARENTHESES_ONLY, true);
		if (store instanceof IPersistentPreferenceStore) {
			try {
				((IPersistentPreferenceStore)store).save();
			} catch (IOException e) {
				ELCorePlugin.getPluginLog().logError(e);
			}
		}
	}
}