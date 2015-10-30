/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.internal;

import java.util.Set;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeCorePreferences {	
	// Preference key
	private static final String ENABLED_DETECTORS = "enabledDetectors"; //$NON-NLS-1$
	private static final String DISABLED_DETECTORS = "disabledDetectors"; //$NON-NLS-1$
	
	private IEclipsePreferences prefs;

	private static RuntimeCorePreferences INSTANCE;
	public static RuntimeCorePreferences getDefault() {
		if( INSTANCE == null )
			INSTANCE = new RuntimeCorePreferences();
		return INSTANCE;
	}
	
	public String[] getEnabledRuntimeDetectors() {
		String enabledDetectors = getPreferences().get(ENABLED_DETECTORS,null);
		return enabledDetectors == null ? null : enabledDetectors.split(","); //$NON-NLS-1$
	}
	public String[] getDisabledRuntimeDetectors() {
		String disabledDetectors = getPreferences().get(DISABLED_DETECTORS,null);
		return disabledDetectors == null ? null : disabledDetectors.split(","); //$NON-NLS-1$
	}
	
	@Deprecated
	public void saveEnabledDetectors() {
		saveDetectorEnablement();
	}
	
	public void saveDetectorEnablement() {
		saveDetectorEnablement(RuntimeCoreActivator.getDefault().getRuntimeDetectors());
	}
	public void saveDetectorEnablement(Set<IRuntimeDetector> detectors) {
		saveEnabledDetectors(detectors);
		saveDisabledDetectors(detectors);
	}
	
	
	
	private void saveDetectors(Set<IRuntimeDetector> allDetectors, String key, boolean enabled) {
		StringBuilder builder = new StringBuilder();
		for (IRuntimeDetector detector:allDetectors) {
			if( (detector.isEnabled() && enabled) || (!detector.isEnabled() && !enabled)){
				builder.append(detector.getId());
				builder.append(","); //$NON-NLS-1$
			}
		}
		String toSave = builder.toString();
		int index = toSave.lastIndexOf(","); //$NON-NLS-1$
		if (index != -1) {
			toSave = toSave.substring(0, index);
		}
		getPreferences().put(key, toSave);
		try {
			getPreferences().flush();
		} catch (BackingStoreException e) {
			RuntimeCoreActivator.pluginLog().logError(e);
		}
	}

	public void saveEnabledDetectors(Set<IRuntimeDetector> allDetectors) {
		saveDetectors(allDetectors, ENABLED_DETECTORS, true);
	}
	
	public void saveDisabledDetectors(Set<IRuntimeDetector> allDetectors) {
		saveDetectors(allDetectors, DISABLED_DETECTORS, false);
	}
	
	IEclipsePreferences getPreferences() {
		if (prefs == null) {
			prefs = ConfigurationScope.INSTANCE.getNode(RuntimeCoreActivator.PLUGIN_ID);
		}
		return prefs;
	}
}
