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

import org.jboss.tools.common.model.options.Preference;

public class ReportPreference extends Preference {
	public static final String OPTIONS_REPORT_PROBLEM_PATH = "%Options%/Struts Studio/Report Problem";
	public static final String ATT_SHOW_ERROR_DIALOG = "showErrorDialog";
	public static final String ATT_SUBMIT_AUTOMATICALLY = "submitProblemAutomatically";
	public static final String ATT_E_MAIL = "eMail";
	public static final String ATT_OTHER = "other";
	public static final String ATT_ATTACH_REDHAT_LOG = "attachRedHatLog";
	public static final String ATT_ATTACH_ECLIPSE_LOG = "attachEclipseLog";

	public static final ReportPreference SHOW_ERROR_DIALOG_OPTION = new ReportPreference(OPTIONS_REPORT_PROBLEM_PATH, ATT_SHOW_ERROR_DIALOG);
	public static final ReportPreference SUBMIT_AUTOMATICALLY_OPTION = new ReportPreference(OPTIONS_REPORT_PROBLEM_PATH, ATT_SUBMIT_AUTOMATICALLY);
	public static final ReportPreference E_MAIL_OPTION = new ReportPreference(OPTIONS_REPORT_PROBLEM_PATH, ATT_E_MAIL);
	public static final ReportPreference OTHER_OPTION = new ReportPreference(OPTIONS_REPORT_PROBLEM_PATH, ATT_OTHER);
	public static final ReportPreference ATTACH_REDHAT_LOG_OPTION = new ReportPreference(OPTIONS_REPORT_PROBLEM_PATH, ATT_ATTACH_REDHAT_LOG);
	public static final ReportPreference ATTACH_ECLIPSE_LOG_OPTION = new ReportPreference(OPTIONS_REPORT_PROBLEM_PATH, ATT_ATTACH_ECLIPSE_LOG);

	protected ReportPreference(String optionPath, String attributeName) {
		super(optionPath, attributeName);
	}

}
