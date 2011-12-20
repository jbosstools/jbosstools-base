/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.HashMap;
import java.util.Map;

import org.jboss.tools.common.preferences.SeverityPreferences;

/**
 * @author Alexey Kazakov
 */
public abstract class ValidationSeverityPreferences extends SeverityPreferences implements IWarningNameMap {

	private Map<String, String[]> warningNameMap = new HashMap<String, String[]>();

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IWarningNameMap#getNames(java.lang.String)
	 */
	@Override
	public String[] getWarningNames(String preferenceID) {
		return warningNameMap.get(preferenceID);
	}

	protected String createSeverityOption(String shortName, String... shortWarningNames) {
		String name = createSeverityOption(shortName);
		String[] parentIDs = getParentWarningGroupIDs();
		String[] allNames = new String[shortWarningNames.length + parentIDs.length + 1];
		for (int i=0; i<shortWarningNames.length; i++) {
			allNames[i] = getWarningGroupID() + "-" + shortWarningNames[i];
		}
		allNames[shortWarningNames.length] = getWarningGroupID();
		for (int i = 0; i < parentIDs.length; i++) {
			allNames[shortWarningNames.length + 1 + i] = parentIDs[i];
		}
		warningNameMap.put(name, allNames);
		return name;
	}

	private static final String[] EMPTY_IDS = new String[0];

	protected String[] getParentWarningGroupIDs() {
		return EMPTY_IDS;
	}

	/**
	 * Returns the ID of the group of warnings managed by this map.
	 * The ID may not contain '-'.
	 * @return
	 */
	public abstract String getWarningGroupID();
}