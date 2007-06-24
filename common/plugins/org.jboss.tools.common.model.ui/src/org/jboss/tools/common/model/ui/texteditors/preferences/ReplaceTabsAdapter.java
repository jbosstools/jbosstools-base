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

import org.jboss.tools.common.meta.constraint.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.text.xml.ui.xpl.BasePreferenceConstants;

public class ReplaceTabsAdapter extends XAdapter {
	static String PROPERTY = BasePreferenceConstants.EDITOR_REPLACE_TAB_WITH_WHITESPACE;  
	
	public ReplaceTabsAdapter() {}

	public String getProperty(XProperty object) {
		if(EditorsPreferencesPage.store == null) return "false";
		return "" + EditorsPreferencesPage.store.getBoolean(PROPERTY);
	}

	public void setProperty(XProperty object, String value) {
		XModelObject o = (XModelObject)object;
		if(!o.isActive()) return;
		if(EditorsPreferencesPage.store == null) return;
		try {
			EditorsPreferencesPage.store.setValue(PROPERTY, "true".equals(value));		
		} catch (Exception e) {}
	}
	
}
