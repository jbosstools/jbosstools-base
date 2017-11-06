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
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.text.xml.ui.xpl.BasePreferenceConstants;

public class TabWidthAdapter extends XAdapter {
	static String PROPERTY = BasePreferenceConstants.EDITOR_TAB_WIDTH;  
	
	public TabWidthAdapter() {}

	public String getProperty(XProperty object) {
		if(EditorsPreferencesPage.store == null) return "4"; //$NON-NLS-1$
		int i = EditorsPreferencesPage.store.getInt(PROPERTY);
		if(i == 0) EditorsPreferencesPage.store.setDefault(PROPERTY, "4"); //$NON-NLS-1$
		return Integer.toString(EditorsPreferencesPage.store.getInt(PROPERTY)); //$NON-NLS-1$
	}
	public void setProperty(XProperty object, String value) {
		XModelObject o = (XModelObject)object;
		if(!o.isActive()) return;
		if(EditorsPreferencesPage.store == null) return;
		int i = 4;
		if(value != null && value.length() > 0) {
			try {
				i = Integer.parseInt(value);
			} catch (NumberFormatException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		EditorsPreferencesPage.store.setValue(PROPERTY, i);		
	}
	
}
