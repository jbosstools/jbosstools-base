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
package org.jboss.common.model.ui.test.preferences;

import junit.framework.TestCase;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.jboss.tools.common.model.ui.preferences.CompanyPreferencesPage;
import org.jboss.tools.common.model.ui.preferences.DecoratorPreferencesPage;
import org.jboss.tools.common.model.ui.preferences.MainPreferencePage;
import org.jboss.tools.common.model.ui.templates.preferences.GlobalTemplatePreferencePage;
import org.jboss.tools.common.model.ui.texteditors.preferences.EditorsPreferencesPage;
import org.jboss.tools.test.util.WorkbenchUtils;
import org.jboss.tools.tests.PreferencePageTest;

/**
 * @author eskimo
 *
 */
public class ModelUiPreferencesPageTest extends PreferencePageTest {

	public void testDecoratorPreferencesPage() {
		doDefaultTest(DecoratorPreferencesPage.ID, DecoratorPreferencesPage.class);
	}

	public void testMainPreferencesPage() {
		doDefaultTest(MainPreferencePage.ID, MainPreferencePage.class);
	}
	
	public void testCompanyPreferencesPage() {
		doDefaultTest(CompanyPreferencesPage.WEB_PREFERENCES_ID,CompanyPreferencesPage.class);
	}
	
	public void testGlobalTemplatesPreferencesPage() {
		doDefaultTest(GlobalTemplatePreferencePage.GLOBAL_TEMPLATES_PREFERENCES_ID,GlobalTemplatePreferencePage.class);
	}
	
	public void testEditorsPreferencesPage() {
		doDefaultTest(EditorsPreferencesPage.EDITOR_PREFERENCES_ID, EditorsPreferencesPage.class);
	}
	
}
