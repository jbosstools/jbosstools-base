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
package org.jboss.tools.common.core.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;

import org.jboss.tools.common.util.HttpUtil;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class HttpUtilTest {

	@Test
	public void testInputStreamReader() throws Exception {
		// Test connections for some available http and https URLs
		InputStreamReader is = HttpUtil.getInputStreamReader("http://code.jquery.com/mobile/1.4.4/jquery.mobile-1.4.4.min.css", 1000);
		assertNotNull(is);
		is = HttpUtil.getInputStreamReader("https://fonts.googleapis.com/css?family=Open+Sans:300,300italic,400,400italic,600,600italic|Noto+Serif:400,400italic,700,700italic|Droid+Sans+Mono:400", 1000);
		assertNotNull(is);
		// And some unavailable
		is = null;
		try {
			is = HttpUtil.getInputStreamReader("xyz://code.jquery.com/mobile/1.4.4/jquery.mobile-1.4.4.min.css", 1000);
		} catch (IOException e) {
		}
		assertNull(is);
		try {
			is = HttpUtil.getInputStreamReader("http://code.jquery.xyz/min.css", 100);
		} catch (IOException e) {
		}
		assertNull(is);
	}

	@Test
	public void testCreateHttpURLConnection() throws Exception {
		// Test connections for some available http and https URLs
		HttpURLConnection connection = HttpUtil.createHttpURLConnection("http://code.jquery.com/mobile/1.4.4/jquery.mobile-1.4.4.min.css", 1000);
		assertNotNull(connection);
		InputStreamReader is = HttpUtil.getInputStreamReader(connection);
		assertNotNull(is);
		// And some unavailable
		connection = null;
		try {
			connection = HttpUtil.createHttpURLConnection("xyz://code.jquery.com/mobile/1.4.4/jquery.mobile-1.4.4.min.css", 1000);
		} catch (IOException e) {
		}
		assertNull(connection);
		connection = HttpUtil.createHttpURLConnection("http://code.jquery.xyz/min.css", 100);
		assertNotNull(connection);
	}
}