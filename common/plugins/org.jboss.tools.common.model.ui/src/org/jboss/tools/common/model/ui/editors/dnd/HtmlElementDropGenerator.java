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
package org.jboss.tools.common.model.ui.editors.dnd;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.wst.html.core.internal.HTMLCorePlugin;
import org.eclipse.wst.html.core.internal.preferences.HTMLCorePreferenceNames;

public class HtmlElementDropGenerator extends DefaultElementGenerator {

	Preferences htmlPreferences = HTMLCorePlugin.getDefault().getPluginPreferences();	
	
	// private methods
	private boolean useLowerCaseForTags() {
		return htmlPreferences.getInt(HTMLCorePreferenceNames.TAG_NAME_CASE)==HTMLCorePreferenceNames.LOWER;
	}
	
	private boolean useLowerCaseForAttributes() {
		return htmlPreferences.getInt(HTMLCorePreferenceNames.ATTR_NAME_CASE)== HTMLCorePreferenceNames.LOWER;
	}
	
	private String convertAttrName(String attrName) {
		return useLowerCaseForAttributes()?attrName.toLowerCase():attrName.toUpperCase();
	}
	
	private String convertTagName(String tagName) {
		return useLowerCaseForTags()?tagName.toLowerCase():tagName.toUpperCase();
	}	

	
	protected String applayAttributePreferences(String attribute) {
		return convertAttrName(attribute);
	}
	
	protected String applayTagPreferences(String tagName) {
		return convertTagName(tagName);
	}	
	
}
