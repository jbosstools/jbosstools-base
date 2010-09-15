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

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.http.HttpRemotePropertiesProvider;
import org.jboss.tools.usage.preferences.GlobalUsageSettings;
import org.junit.Test;

/**
 * Test for the global usage report settings. All tests are disabled yet,
 * implementation's unfortunately still buggy.
 */
public class GlobalUsageSettingsTest {

	@Test
	public void canExtractEnabledValue() throws IOException {
		GlobalUsageSettingsFake reportSettings = new GlobalUsageSettingsFake("true", "", "");
		assertTrue(reportSettings.isAllInstancesReportingEnabled());
	}

	@Test
	public void canExtractDisabledValue() throws IOException {
		GlobalUsageSettingsFake reportSettings = new GlobalUsageSettingsFake("false", "", "");
		assertFalse(reportSettings.isAllInstancesReportingEnabled());
	}

	@Test
	public void canExtractDisabledOutUndefinedValue() throws IOException {
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("Rubbish", "", "");
		assertFalse(reportEnablement.isAllInstancesReportingEnabled());
	}

	@Test
	public void instanceReportingIsEnabledIfSysPropIsNotSet() throws IOException {
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("", "", "");
		assertTrue(reportEnablement.isInstanceReportingEnabled());
	}

	@Test
	public void instanceReportingIsDisabledIfSysPropIsFalse() throws IOException {
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("", "", "");
		System.setProperty(GlobalUsageSettings.SYSPROPS_INSTANCE_ENABLED_KEY, Boolean.FALSE.toString());
		assertFalse(reportEnablement.isInstanceReportingEnabled());
	}

	@Test
	public void instanceReportingIsEnabledIfSysPropIsTrue() throws IOException {
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("", "", "");
		System.setProperty(GlobalUsageSettings.SYSPROPS_INSTANCE_ENABLED_KEY, Boolean.TRUE.toString());
		assertTrue(reportEnablement.isInstanceReportingEnabled());
	}

	private class GlobalUsageSettingsFake extends GlobalUsageSettings {

		private String allInstancesEnabledValue;
		private String integerValue;
		private String stringValue;

		public GlobalUsageSettingsFake(String allInstancesEnabledValue, String dummyValue, String anotherValue)
				throws IOException {
			super(JBossToolsUsageTestActivator.getDefault());
			this.allInstancesEnabledValue = allInstancesEnabledValue;
			this.stringValue = dummyValue;
			this.integerValue = anotherValue;
		}

		@Override
		protected HttpRemotePropertiesProvider createRemoteMap(String url, char valueDelimiter, Plugin plugin,
				String... keys) {
			return new HttpRemotePropertiesProvider(url, valueDelimiter, plugin, keys) {
				@Override
				protected InputStreamReader request(HttpURLConnection urlConnection)
						throws UnsupportedEncodingException {
					return new InputStreamReader(new ByteArrayInputStream(
							getRemotePropertiesRawData(
									allInstancesEnabledValue
									, stringValue
									, integerValue).getBytes())
							, "UTF-8");
				}
			};
		}
	}

	private String getRemotePropertiesRawData(String enablementValue, String dummyValue, String integerValue) {

		return "some rubbish at the beginning..."
				+ GlobalUsageSettings.REMOTEPROPS_ALLINSTANCES_ENABLED_KEY
				+ enablementValue
				+ "\n"
				+ "#"
				+ "some rubbish at the end";

	}

	@Test
	public void isPageAccessible() throws IOException {
		GlobalUsageSettings reportEnablement = new GlobalUsageSettings(JBossToolsUsageTestActivator
				.getDefault());
		System.err.println("Usage reporting is globally \"" + reportEnablement.isAllInstancesReportingEnabled() + "\"");
	}
}
