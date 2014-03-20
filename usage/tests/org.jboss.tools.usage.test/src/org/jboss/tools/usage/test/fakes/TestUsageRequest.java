/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.reporting.UsageRequest;
import org.jboss.tools.usage.test.JBossToolsTestBranding;

/**
 * @author Alexey Kazakov
 */
public class TestUsageRequest extends UsageRequest {

	private IJBossToolsEclipseEnvironment testEnvironment = new ReportingEclipseEnvironmentFake(
			JBossToolsTestBranding.GOOGLE_ANALYTICS_TEST_ACCOUNT,
			JBossToolsTestBranding.REPORTING_HOST,
			ReportingEclipseEnvironmentFake.JAVA_VERSION,
			new EclipsePreferencesFake(),
			new EclipseUserAgentFake());

	public TestUsageRequest() {
		super(null);
		this.environment = testEnvironment;
	}
}