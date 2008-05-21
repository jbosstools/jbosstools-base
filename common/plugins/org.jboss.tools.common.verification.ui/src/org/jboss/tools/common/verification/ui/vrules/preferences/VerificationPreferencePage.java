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
package org.jboss.tools.common.verification.ui.vrules.preferences;

import org.jboss.tools.common.model.ui.preferences.TabbedPreferencesPage;
import org.jboss.tools.common.model.ui.preferences.XMOBasedPreferencesPage;

import org.jboss.tools.common.model.XModelObject;

public class VerificationPreferencePage extends TabbedPreferencesPage {
	public static final String PREFERENCES[] = {
		"%Options%/Struts Studio/Verification" 
	};
	public static String ATTR_ERRORS_NUMBER_LIMIT = "Reported Errors Number Limit";
	
	public VerificationPreferencePage() {
		addPreferencePage(new RulesConfigurationPage());
		XModelObject o = getPreferenceModel().getByPath(PREFERENCES[0]);
		if(o != null) {
			XMOBasedPreferencesPage page = new ReportedErrorsPage(o);
			addPreferencePage(page);
		}
	}
	
	class ReportedErrorsPage extends XMOBasedPreferencesPage {
		public ReportedErrorsPage(XModelObject o) {
			super(o);
		}
		public String getTitle() {
			return "Options";
		}
	}
	
}
