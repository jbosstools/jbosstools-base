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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.http.HttpRemotePropertiesProvider;
import org.jboss.tools.usage.http.IPropertiesProvider;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;
import org.junit.Test;

/**
 * Test for the global usage report settings. All tests are disabled yet,
 * implementation's unfortunately still buggy.
 */
public class GlobalUsageSettingsTest {

	/**
	 * <ul>
	 * <li>sys prop: <code>not set</code></li>
	 * <li>remote prop: <code>not set</code></li>
	 * </ul>
	 * 
	 * <p>
	 * <code>disabled</code>
	 * <p>
	 */
	@Test
	public void reportingIsENABLEDIfSysPropIsNOTSETAndRemotePropIsNOTSET() throws IOException {
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("");
		assertFalse(reportEnablement.isReportingEnabled());
	}

	/**
	 * <ul>
	 * <li>sys prop: <code>false</code></li>
	 * <li>remote prop: <code>not set</code></li>
	 * </ul>
	 * 
	 * <p>
	 * <code>disabled</code>
	 * <p>
	 */
	@Test
	public void instanceReportingIsDISABLEDIfSysPropIsFALSEAndRemotePropIsNOTSET() throws IOException {
		System.setProperty(GlobalUsageSettings.USAGE_REPORTING_ENABLED_KEY, Boolean.FALSE.toString());
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("");
		assertFalse(reportEnablement.isReportingEnabled());
	}

	/**
	 * <ul>
	 * <li>sys prop: <code>true</code></li>
	 * <li>remote prop: <code>false</code></li>
	 * </ul>
	 * 
	 * <p>
	 * <code>disabled</code>
	 * <p>
	 */
	@Test
	public void instanceReportingIsENABLEDIfSysPropIsTRUEAndRemotePropIsFALSE() throws IOException {
		System.setProperty(GlobalUsageSettings.USAGE_REPORTING_ENABLED_KEY, Boolean.TRUE.toString());
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("false");
		assertFalse(reportEnablement.isReportingEnabled());
	}

	/**
	 * <ul>
	 * <li>sys prop: <code>not set</code></li>
	 * <li>remote prop: <code>false</code></li>
	 * </ul>
	 * 
	 * <p>
	 * <code>disabled</code>
	 * <p>
	 */
	@Test
	public void instanceReportingIsENABLEDIfSysPropIsNOTSETAndRemotePropIsTRUE() throws IOException {
		GlobalUsageSettingsFake reportEnablement = new GlobalUsageSettingsFake("");
		System.setProperty(GlobalUsageSettings.USAGE_REPORTING_ENABLED_KEY, Boolean.FALSE.toString());
		assertFalse(reportEnablement.isReportingEnabled());
	}

	private class GlobalUsageSettingsFake extends GlobalUsageSettings {

		private String usageReportingEnabled;

		public GlobalUsageSettingsFake(String usageReportingEnabled)
				throws IOException {
			super(JBossToolsUsageTestActivator.getDefault());
			this.usageReportingEnabled = usageReportingEnabled;
		}

		@Override
		protected IPropertiesProvider createRemoteMap(String url, char valueDelimiter, Plugin plugin,
				String... keys) {
			return new HttpRemotePropertiesProvider(url, valueDelimiter, new UsagePluginLogger(plugin), keys) {
				@Override
				protected InputStreamReader request(HttpURLConnection urlConnection)
						throws UnsupportedEncodingException {
					return new InputStreamReader(
							new ByteArrayInputStream(getRemotePropertiesRawData(usageReportingEnabled).getBytes())
							, "UTF-8");
				}
			};
		}
	}

	private String getRemotePropertiesRawData(String enablementValue) {

		return "some rubbish at the beginning..."
				+ GlobalUsageSettings.REMOTEPROPS_USAGE_REPORTING_ENABLED_KEY
				+ enablementValue
				+ "\n"
				+ "#"
				+ "some rubbish at the end";

	}

	@Test
	public void isPageAccessible() throws IOException {
		GlobalUsageSettings reportEnablement = new GlobalUsageSettings(JBossToolsUsageTestActivator
				.getDefault());
		System.err.println("Usage reporting is globally \"" + reportEnablement.isReportingEnabled() + "\"");
	}
}
