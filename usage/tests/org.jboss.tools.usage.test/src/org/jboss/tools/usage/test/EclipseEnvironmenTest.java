/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
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
import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.internal.EclipseEnvironment;
import org.junit.Test;

public class EclipseEnvironmenTest {

	private static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";
	private static final String HOSTNAME = "jboss.org";
	private static final String REFERRAL = "0";
	private static final String LOCALE_US = "en_US";

	@Test
	public void testMacOs() throws UnsupportedEncodingException {
		EclipseEnvironment eclipseEnvironment = new EclipseEnvironmentFake(GANALYTICS_ACCOUNTNAME, HOSTNAME, REFERRAL,
				Platform.OS_MACOSX, LOCALE_US);
		String userAgent = eclipseEnvironment.getUserAgent();
		assertApplicationNameAndVersion("com.jboss.jbds.product", "3.0.1", userAgent);
		assertOs("Macintosh", "Intel Mac OS X 10.5", userAgent);
		assertLanguage("en-US", userAgent);
	}

	@Test
	public void testLinux() throws UnsupportedEncodingException {
		EclipseEnvironment eclipseEnvironment = new EclipseEnvironmentFake(GANALYTICS_ACCOUNTNAME, HOSTNAME, REFERRAL,
				Platform.OS_LINUX, LOCALE_US);
		String userAgent = eclipseEnvironment.getUserAgent();
		assertApplicationNameAndVersion("com.jboss.jbds.product", "3.0.1", userAgent);
		assertOs("X11", "Linux i686", userAgent);
		assertLanguage("en-US", userAgent);
	}

	@Test
	public void testWindows() throws UnsupportedEncodingException {
		EclipseEnvironment eclipseEnvironment = new EclipseEnvironmentFake(GANALYTICS_ACCOUNTNAME, HOSTNAME, REFERRAL,
				Platform.OS_WIN32, LOCALE_US);
		String userAgent = eclipseEnvironment.getUserAgent();
		assertApplicationNameAndVersion("com.jboss.jbds.product", "3.0.1", userAgent);
		assertOs("Windows", "Windows NT 6.1", userAgent);
		assertLanguage("en-US", userAgent);
	}

	private void assertApplicationNameAndVersion(String applicationName, String applicationVersion, String userAgent) {
		Matcher matcher = Pattern.compile("([a-zA-Z\\.]+)/([0-9\\.]+).+").matcher(userAgent);
		assertTrue(matcher.matches());
		assertEquals(2, matcher.groupCount());
		assertEquals(applicationName, matcher.group(1));
		assertEquals(applicationVersion, matcher.group(2));
	}

	private void assertOs(String platform, String os, String userAgent) {
		Matcher matcher = Pattern.compile(".+ \\((.+); U; (.+); .+\\)").matcher(userAgent);
		assertTrue(matcher.matches());
		assertEquals(2, matcher.groupCount());
		assertEquals(platform, matcher.group(1));
		assertEquals(os, matcher.group(2));
	}

	private void assertLanguage(String language, String userAgent) {
		Matcher matcher = Pattern.compile(".+ \\(.+; U; .+ .+; (.+)\\)").matcher(userAgent);
		assertTrue(matcher.matches());
		assertEquals(1, matcher.groupCount());
		assertEquals(language, matcher.group(1));
	}
}
