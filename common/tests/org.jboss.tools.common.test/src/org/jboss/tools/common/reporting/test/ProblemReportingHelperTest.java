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
package org.jboss.tools.common.reporting.test;

import junit.framework.TestCase;

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.reporting.ProblemReportingHelper;

/**
 * @author eskimo
 *
 */
public class ProblemReportingHelperTest extends TestCase {
	
	boolean testPassed = false;
	


	@Override
	protected void setUp() throws Exception {
		// TODO Auto-generated method stub
		super.setUp();
		testPassed = false;
		CommonPlugin.getInstance().getLog().addLogListener(new ILogListener() {
			public void logging(IStatus status, String plugin) {
				testPassed = true;
				
			}});
	}

	/**
	 * Test method for {@link org.jboss.tools.common.reporting.ProblemReportingHelper#reportProblem(java.lang.String, java.lang.Throwable)}.
	 */
	public void testReportProblemStringThrowable() {
		ProblemReportingHelper.reportProblem(CommonPlugin.PLUGIN_ID, new Throwable("Message"));
		assertTrue(testPassed);
	}

	/**
	 * Test method for {@link org.jboss.tools.common.reporting.ProblemReportingHelper#reportProblem(java.lang.String, java.lang.String, java.lang.Throwable)}.
	 */
	public void testReportProblemStringStringThrowable() {
		ProblemReportingHelper.reportProblem(CommonPlugin.PLUGIN_ID, "Message", new Throwable("Message"));
		assertTrue(testPassed);
	}

	/**
	 * Test method for {@link org.jboss.tools.common.reporting.ProblemReportingHelper#reportProblem(org.eclipse.core.runtime.IStatus)}.
	 */
	public void testReportProblemIStatus() {
		ProblemReportingHelper.reportProblem(Status.OK_STATUS);
		assertTrue(testPassed);
	}

}
