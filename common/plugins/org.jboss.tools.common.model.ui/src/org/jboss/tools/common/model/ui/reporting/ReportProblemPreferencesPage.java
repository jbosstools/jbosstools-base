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
package org.jboss.tools.common.model.ui.reporting;

import org.jboss.tools.common.model.ui.preferences.XMOBasedPreferencesPage;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.jboss.tools.common.model.XModelObject;

public class ReportProblemPreferencesPage extends XMOBasedPreferencesPage {
	static XModelObject getReportProblemObject() {
		return ModelUtilities.getPreferenceModel().getByPath(ReportPreference.OPTIONS_REPORT_PROBLEM_PATH);
	}

	public ReportProblemPreferencesPage() {
		super(getReportProblemObject());
	}

}
