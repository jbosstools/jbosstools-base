/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.googleanalytics.eclipse.IEclipseUserAgent;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferences;
import org.jboss.tools.usage.internal.reporting.UsageReport;
import org.jboss.tools.usage.test.fakes.EclipseUserAgentFake;
import org.jboss.tools.usage.test.fakes.ReportingEclipseEnvironmentFake;
import org.junit.Test;

public class UsageReportIntegrationTest {

	private CountDownLatch latch;

	private final class UsageReportFake extends UsageReport {

		private boolean enabled;

		public UsageReportFake(boolean enabled, IEclipseUserAgent userAgent) {
			this(enabled, new ReportingEclipseEnvironmentFake(userAgent));
		}

		public UsageReportFake(boolean enabled, IJBossToolsEclipseEnvironment eclipseEnvironment) {
			super(new JBossToolsTestsFocusPoint("UsageReportIntegrationTest"),
					eclipseEnvironment,
					new GlobalUsageSettings(JBossToolsUsageTestActivator.getDefault()));
			latch = new CountDownLatch(1);
			this.enabled = enabled;
		}

		@Override
		protected void doReport() {
			try {
				super.doReport();
			} finally {
				latch.countDown();
			}
		}

		@Override
		protected Boolean askUserForEnablement() {
			return enabled;
		}

		@Override
		protected boolean isAskUser() {
			// has to be true, askUserForEnablement is not queried otherwise
			return true;
		}

		@Override
		protected boolean isReportingGloballyEnabled() {
			// has to be true, askUserForEnablement is not queried otherwise
			return true;
		}
	}

	@Test
	public void shouldReportToTestAccount() throws InterruptedException {
		UsageReportPreferences.setEnabled(true);
		EclipseUserAgentFake userAgent = new EclipseUserAgentFake(
				EclipseUserAgentFake.LOCALE_US,
				EclipseUserAgentFake.OS_LINUX,
				EclipseUserAgentFake.VERSION_LINUX_FEDORA13,
				EclipseUserAgentFake.PROP_SUN_ARCH_32);
		new UsageReportFake(true, userAgent).report();
		latch.await(300l, TimeUnit.SECONDS);
	}
}
