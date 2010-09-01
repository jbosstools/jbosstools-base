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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;

import org.jboss.tools.usage.preferences.GlobalUsageReportingSettings;
import org.junit.Test;

/**
 * Test for the global usage report settings. All tests are disabled yet,
 * implementation's unfortunately still buggy.
 */
public class GlobalUsageReportingSettingsTest {

	@Test
	public void canExtractEnabledValue() throws IOException {
		GlobalReportingSettingsFake reportSettings = new GlobalReportingSettingsFake("true", "", "");
		assertTrue(reportSettings.isEnabled());
	}

	@Test
	public void canExtractDisabledValue() throws IOException {
		GlobalReportingSettingsFake reportSettings = new GlobalReportingSettingsFake("false", "", "");
		assertFalse(reportSettings.isEnabled());
	}

	@Test
	public void canExtractDisabledOutUndefinedValue() throws IOException {
		GlobalReportingSettingsFake reportEnablement = new GlobalReportingSettingsFake("Rubbish", "", "");
		assertFalse(reportEnablement.isEnabled());
	}

	private class GlobalReportingSettingsFake extends GlobalUsageReportingSettings {

		private String enablementValue;
		private String integerValue;
		private String stringValue;

		public GlobalReportingSettingsFake(String enablementValue, String dummyValue, String anotherValue)
				throws IOException {
			super(JBossToolsUsageTestActivator.getDefault());
			this.enablementValue = enablementValue;
			this.stringValue = dummyValue;
			this.integerValue = anotherValue;
		}

		@Override
		protected InputStreamReader request(HttpURLConnection urlConnection) throws UnsupportedEncodingException {
			return new InputStreamReader(new ByteArrayInputStream(getEnablementPageContent(enablementValue,
					stringValue, integerValue).getBytes()), "UTF-8");
		}
	}

	private String getEnablementPageContent(String enablementValue, String dummyValue, String integerValue) {

		return "some rubbish at the beginning..."
				+ GlobalUsageReportingSettings.REPORT_ENABLEMENT_KEY
				+ enablementValue
				+ "\n"
				+ "#"
				+ "some rubbish at the end";

	}

	@Test
	public void isPageAccessible() throws IOException {
		GlobalUsageReportingSettings reportEnablement = new GlobalUsageReportingSettings(JBossToolsUsageTestActivator
				.getDefault());
		System.err.println("Usage reporting is globally \"" + reportEnablement.isEnabled() + "\"");
	}
}
