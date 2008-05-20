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

/**
 * This class manages IProblemReporter implementations. 
 * @author glory
 */
public class ProblemReporterFactory {
	private static ProblemReporterFactory instance = new ProblemReporterFactory();
	
	public static ProblemReporterFactory getInstance() {
		return instance;
	}
	IProblemReporter defaultReporter = new DefaultProblemReporter();
	IProblemReporter reporter = null;
	
	public void setReporter(IProblemReporter reporter) {
		this.reporter = reporter;
	}
	
	public IProblemReporter getProblemReporter() {
		return reporter == null ? defaultReporter : reporter;
	}
	
	public IProblemReporter getDefaultProblemReporter() {
		return defaultReporter;
	}
	

}
