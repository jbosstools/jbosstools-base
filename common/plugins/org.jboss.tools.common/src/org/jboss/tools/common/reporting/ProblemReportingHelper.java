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
package org.jboss.tools.common.reporting;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Helper class for processing all exceptions caught in Red Hat code.  
 * @author glory
 */
public class ProblemReportingHelper {
	/**
	 * Method for processing a throwable caught in Red Hat code. 
	 * @param plugin
	 * @param throwable
	 */
	public static void reportProblem(String plugin, Throwable throwable) {
		String message = (throwable == null) ? null : throwable.getMessage();
		reportProblem(plugin, message, throwable);
	}

	/**
	 * Method for processing a throwable caught in Red Hat code. 
	 * @param plugin
	 * @param message
	 * @param throwable
	 */
	public static void reportProblem(String plugin, String message, Throwable throwable) {
		if(throwable == null) {
			throwable = new NullPointerException("Parameter throwable is null.");
			reportProblem("org.jboss.tools.common", throwable);
		} else {		
			if(message == null) {
				message = throwable.getMessage();
			}
			if(message==null) {
				message = "";
			}
			IStatus status = new Status(IStatus.ERROR, plugin, 0, message, throwable);
			reportProblem(status);
		}
	}

	/**
	 * Method for processing a throwable caught in Red Hat code. 
	 * @param status
	 */
	public static void reportProblem(IStatus status) {
		if(status == null) {
			reportProblem("org.jboss.tools.common", new NullPointerException("Parameter status is null."));
			return;
		}
		IProblemReporter reporter = ProblemReporterFactory.getInstance().getProblemReporter();
		try {
			reporter.reportProblem(status);
		} catch (Exception t) {
			IProblemReporter defaultReporter = ProblemReporterFactory.getInstance().getDefaultProblemReporter();
			if(defaultReporter != reporter) {
				defaultReporter.reportProblem(status);
				status = new Status(IStatus.ERROR, "org.jboss.tools.common", 0, "Failed to call custom reporter.", t);
				defaultReporter.reportProblem(status);
			}
		}
	}
	
	public static final ProblemBuffer buffer = new ProblemBuffer();

}
