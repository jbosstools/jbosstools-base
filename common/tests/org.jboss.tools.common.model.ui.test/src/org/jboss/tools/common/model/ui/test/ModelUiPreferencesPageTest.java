/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.test;

import junit.framework.TestCase;

import org.eclipse.jface.preference.PreferenceDialog;
import org.jboss.tools.common.model.ui.preferences.CompanyPreferencesPage;
import org.jboss.tools.common.model.ui.templates.preferences.GlobalTemplatePreferencePage;
import org.jboss.tools.common.model.ui.texteditors.preferences.EditorsPreferencesPage;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * @author eskimo
 *
 */
public class ModelUiPreferencesPageTest extends TestCase {
	
	public void testWebPreferencesPage() {
		PreferenceDialog prefDialog = 
			WorkbenchUtils.createPreferenceDialog(
					CompanyPreferencesPage.WEB_PREFERENCES_ID);

		try {
			prefDialog.setBlockOnOpen(false);
			prefDialog.open();
			
			Object selectedPage = prefDialog.getSelectedPage();
			assertTrue("Selected page is not an instance of CompanyPreferencesPage", selectedPage instanceof CompanyPreferencesPage);
		} finally {
			prefDialog.close();
		}
	}
	
	public void testEditorsPreferencesPage() {
		PreferenceDialog prefDialog = 
			WorkbenchUtils.createPreferenceDialog(
					EditorsPreferencesPage.EDITOR_PREFERENCES_ID);

		try {
			prefDialog.setBlockOnOpen(false);
			prefDialog.open();
			
			Object selectedPage = prefDialog.getSelectedPage();
			assertTrue("Selected page is not an instance of EditorsPreferencesPage", selectedPage instanceof EditorsPreferencesPage);
		} finally {
			prefDialog.close();
		}
	}
	
	public void testGlobalTemplatesPreferencesPage() {
		PreferenceDialog prefDialog = 
			WorkbenchUtils.createPreferenceDialog(
					GlobalTemplatePreferencePage.GLOBAL_TEMPLATES_PREFERENCES_ID);

		try {
			prefDialog.setBlockOnOpen(false);
			prefDialog.open();
			
			Object selectedPage = prefDialog.getSelectedPage();
			assertTrue("Selected page is not an instance of GlobalTemplatePreferencePage", selectedPage instanceof GlobalTemplatePreferencePage);
		} finally {
			prefDialog.close();
		}
	}
}
