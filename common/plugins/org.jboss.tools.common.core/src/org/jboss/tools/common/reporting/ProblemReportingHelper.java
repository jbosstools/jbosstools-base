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
		String message = (throwable == null) ? null : throwable.getLocalizedMessage();
		reportProblem(plugin, message, throwable);
	}

	/**
	 * Method for processing a throwable caught in Red Hat code. 
	 * @param plugin
	 * @param message (translatable)
	 * @param throwable
	 */
	public static void reportProblem(String plugin, String message, Throwable throwable) {
		if(message==null) {
			throw new IllegalArgumentException("Message parameter cannot be null"); //$NON-NLS-1$
		}
		IStatus status = new Status(IStatus.ERROR, plugin, 0, message, throwable);
		reportProblem(status);
	}

	/**
	 * Method for processing a throwable caught in Red Hat code. 
	 * @param status
	 */
	public static void reportProblem(IStatus status) {
		if(status == null) {
			reportProblem("org.jboss.tools.common",  //$NON-NLS-1$
					new IllegalArgumentException("Parameter 'status' cannot be null")); //$NON-NLS-1$
			return;
		}
		IProblemReporter reporter = ProblemReporterFactory.getInstance().getProblemReporter();
		reporter.reportProblem(status);
	}
	
	public static final ProblemBuffer buffer = new ProblemBuffer();

}
