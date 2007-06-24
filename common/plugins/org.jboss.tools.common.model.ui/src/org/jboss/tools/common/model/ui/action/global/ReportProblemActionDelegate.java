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
package org.jboss.tools.common.model.ui.action.global;

import java.util.Properties;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.reporting.ReportProblemWizard;

public class ReportProblemActionDelegate implements IWorkbenchWindowActionDelegate {
	
	public ReportProblemActionDelegate() {}

	public void dispose() {}

	public void init(IWorkbenchWindow window) {}

	public void run(IAction action) {
		ClassLoaderUtil.init();
		try {
			ReportProblemWizard wizard = new ReportProblemWizard();
			Properties p = new Properties();
			p.setProperty("help", "ReportProblemWizard");
			wizard.setObject(p);
			wizard.execute();
		} catch (Exception t) {
			String message = "Report Problems Wizard failed.";
			IStatus status = new Status(IStatus.ERROR, "org.jboss.tools.common.model.ui", 0, message, t);
			ProblemReportingHelper.reportProblem(status);
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {}

//	protected String getUrl() {
//		return "http://www.redhat.com/strutsfeedback.htm";
//	}

}
