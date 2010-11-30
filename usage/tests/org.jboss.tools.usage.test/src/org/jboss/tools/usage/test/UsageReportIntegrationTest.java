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

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferences;
import org.jboss.tools.usage.internal.reporting.UsageReport;
import org.junit.Test;

public class UsageReportIntegrationTest {

	private CountDownLatch latch;

	private final class UsageReportFake extends UsageReport {

		private boolean enabled;

		public UsageReportFake(boolean enabled) {
			super(new JBossToolsTestsFocusPoint("UsageReportIntegrationTest"),
					JBossToolsUsageActivator.getDefault().getJBossToolsEclipseEnvironment(),
					new GlobalUsageSettings(JBossToolsUsageActivator.getDefault()));
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
	public void doesEnableInPreferences() throws InterruptedException {
		UsageReportPreferences.setEnabled(false);
		new UsageReportFake(true).report();
		latch.await(10l, TimeUnit.SECONDS);
		assertTrue(UsageReportPreferences.isEnabled());
	}

	@Test
	public void doesDisableInPreferences() throws InterruptedException {
		UsageReportPreferences.setEnabled(true);
		new UsageReportFake(false).report();
		latch.await(10l, TimeUnit.SECONDS);
		assertFalse(UsageReportPreferences.isEnabled());
	}
}
