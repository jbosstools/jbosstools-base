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