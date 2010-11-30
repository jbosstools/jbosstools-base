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

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;

import org.jboss.tools.usage.http.HttpRemotePropertiesProvider;
import org.jboss.tools.usage.http.IPropertiesProvider;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;
import org.junit.Test;

/**
 * Test for the global usage report settings. All tests are disabled yet,
 * implementation's unfortunately still buggy.
 */
public class HttpRemotePropertiesTest {

	private static final String KEY = "usage_reporting_enabled="; //$NON-NLS-1$

	/** the delimiter that delimits the key/value-pairs */
	private static final char VALUE_DELIMITER = '\n';

	@Test
	public void canExtractTrueValue() throws IOException {
		IPropertiesProvider propertiesProvider = createHttpPropertiesProvider("true", "", "", VALUE_DELIMITER, KEY);
		assertEquals("true", propertiesProvider.getMap().get(KEY));
	}

	@Test
	public void canExtractFalseValue() throws IOException {
		IPropertiesProvider propertiesProvider = createHttpPropertiesProvider("false", "", "", VALUE_DELIMITER, KEY);
		assertEquals("false", propertiesProvider.getMap().get(KEY));
	}

	@Test
	public void canExtractRubbish() throws IOException {
		IPropertiesProvider propertiesProvider = createHttpPropertiesProvider("Rubbish", "", "", VALUE_DELIMITER, KEY);
		assertEquals("Rubbish", propertiesProvider.getMap().get(KEY));
	}

	private IPropertiesProvider createHttpPropertiesProvider(final String booleanValue,
			final String stringValue, final String anotherValue, char valueDelimiter, String... keys) {

		return new HttpRemotePropertiesProvider("http://dummy", valueDelimiter,
					new UsagePluginLogger(JBossToolsUsageTestActivator.getDefault()), keys) {
			@Override
			protected InputStreamReader request(HttpURLConnection urlConnection)
						throws UnsupportedEncodingException {
				return new InputStreamReader(new ByteArrayInputStream(
							getRemotePropertiesRawData(
									booleanValue
									, stringValue
									, anotherValue).getBytes())
							, "UTF-8");
			}
		};
	}

	private String getRemotePropertiesRawData(String enablementValue, String dummyValue, String integerValue) {

		return "some rubbish at the beginning..."
				+ KEY
				+ enablementValue
				+ "\n"
				+ "#"
				+ "some rubbish at the end";
	}
}
