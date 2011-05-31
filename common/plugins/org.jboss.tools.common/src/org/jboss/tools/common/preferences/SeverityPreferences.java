/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.preferences;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.CommonPlugin;

/**
 * Constants for names of seam preferences.
 * Static accesses to seam preferences.
 * 
 * Framework for Severity preferences.
 * 1) Create a class which extends SeverityPreferences
 * 2) Add constant using static method createSeverityOption(String)
 *    Put it under relevant section, e.g. //components, or create new section
 *    in the sub-class
 * 3) Add in messages.properties in core plug-in of the sub-class 
 *    error message with the same name
 * 4) Add constant and property named <ID>ValidatorConfigurationBlock_pb_%name%_label,
 *    where %name% is produced from constant name in Preferences like AAA_BBB_CCC -> aaaBbbCcc
 *    to <ID>PreferencesMessages.java
 *    and <ID>PreferencesMessages.properties
 *    in UI plug-in.
 *    Put these entries under relevant section. For a new section add constant and property 
 *    <ID>ValidatorConfigurationBlock_section_%newSectionName%
 * 5) In class <ID>ValidatorConfigurationBlock (in UI plug-in)
 *    modify SectionDescription constants, according to instruction there.
 * 
 * @author Viacheslav Kabanovich, Alexey Kazakov
 */
public abstract class SeverityPreferences {

	public static final String ENABLE_BLOCK_PREFERENCE_NAME = "enableBlock"; //$NON-NLS-1$
	public static final String MAX_NUMBER_OF_MARKERS_PREFERENCE_NAME = CommonPlugin.PLUGIN_ID + ".validator.problem.markersBlock"; //$NON-NLS-1$
	public static final int DEFAULT_MAX_NUMBER_OF_MARKERS_PER_FILE = 20;
	public static final String WRONG_BUILDER_ORDER_PREFERENCE_NAME = CommonPlugin.PLUGIN_ID + ".validator.problem.wrongBuilderOrder"; //$NON-NLS-1$

	public static final String ERROR = "error"; //$NON-NLS-1$
	public static final String WARNING = "warning"; //$NON-NLS-1$
	public static final String IGNORE = "ignore"; //$NON-NLS-1$

	public static final String ENABLE = JavaCore.ENABLED;
	public static final String DISABLE = JavaCore.DISABLED;

	abstract protected Set<String> getSeverityOptionNames();

	abstract protected String createSeverityOption(String shortName);

	abstract protected String getPluginId();

	public IEclipsePreferences getProjectPreferences(IProject project) {
		return new ProjectScope(project).getNode(getPluginId());
	}

	public IEclipsePreferences getDefaultPreferences() {
		return new DefaultScope().getNode(getPluginId());
	}

	public IEclipsePreferences getInstancePreferences() {
		return new InstanceScope().getNode(getPluginId());
	}

	public String getProjectPreference(IProject project, String key) {
		IEclipsePreferences p = getProjectPreferences(project);
		if(p == null) {
			return null;
		}
		String value = p.get(key, null);
		return value != null ? value : getInstancePreference(key);
	}

	public int getMaxNumberOfProblemMarkersPerResource(IProject project) {
		IEclipsePreferences p = getProjectPreferences(project);
		if(p == null) {
			return 0;
		}
		String value = p.get(MAX_NUMBER_OF_MARKERS_PREFERENCE_NAME, null);
		if(value!=null) {
			return p.getInt(MAX_NUMBER_OF_MARKERS_PREFERENCE_NAME, 0);
		}
		p = getInstancePreferences();
		value = p == null ? null : p.get(MAX_NUMBER_OF_MARKERS_PREFERENCE_NAME, null);
		if(value!=null) {
			return p.getInt(MAX_NUMBER_OF_MARKERS_PREFERENCE_NAME, 0);
		}
		p = getDefaultPreferences();
		return p.getInt(MAX_NUMBER_OF_MARKERS_PREFERENCE_NAME, 0);
	}

	public boolean isEnabled(IProject project) {
		IEclipsePreferences p = getProjectPreferences(project);
		if(p == null) {
			return false;
		}
		String value = p.get(ENABLE_BLOCK_PREFERENCE_NAME, null);
		if(value!=null) {
			return p.getBoolean(ENABLE_BLOCK_PREFERENCE_NAME, false);
		}
		p = getInstancePreferences();
		value = p == null ? null : p.get(ENABLE_BLOCK_PREFERENCE_NAME, null);
		if(value!=null) {
			return p.getBoolean(ENABLE_BLOCK_PREFERENCE_NAME, false);
		}
		p = getDefaultPreferences();
		return p.getBoolean(ENABLE_BLOCK_PREFERENCE_NAME, false);
	}

	public String getBuilderOrderPreference(IProject project) {
		IEclipsePreferences p = getProjectPreferences(project);
		if(p == null) {
			return null;
		}
		String value = p.get(WRONG_BUILDER_ORDER_PREFERENCE_NAME, null);
		if(value != null) {
			return value;
		}
		p = getInstancePreferences();
		value = p == null ? null : p.get(WRONG_BUILDER_ORDER_PREFERENCE_NAME, null);
		if(value != null) {
			return value;
		}
		p = getDefaultPreferences();
		return p.get(WRONG_BUILDER_ORDER_PREFERENCE_NAME, null);
	}

	public String getInstancePreference(String key) {
		IEclipsePreferences p = getInstancePreferences();
		String value = p == null ? null : p.get(key, null);
		return value != null ? value : getDefaultPreference(key);
	}

	public String getDefaultPreference(String key) {
		IEclipsePreferences p = getDefaultPreferences();
		if(p == null) {
			return null;
		}
		return p.get(key, null);
	}
}