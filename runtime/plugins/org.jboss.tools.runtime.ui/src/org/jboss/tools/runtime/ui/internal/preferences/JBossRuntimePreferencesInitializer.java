/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal.preferences;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * @author snjeza
 * 
 */
public class JBossRuntimePreferencesInitializer extends
		AbstractPreferenceInitializer {
	public static final String LASTPATH = "lastPath"; //$NON-NLS-1$
	public static final String FIRST_START = "firstStart"; //$NON-NLS-1$

	@Override
	public void initializeDefaultPreferences() {
		Preferences preferences = RuntimeUIActivator.getDefault().getPluginPreferences();
		preferences.setDefault( FIRST_START, true);
	}

}
