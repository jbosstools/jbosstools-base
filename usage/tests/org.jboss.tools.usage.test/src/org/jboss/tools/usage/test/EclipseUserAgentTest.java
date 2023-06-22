/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.test.fakes.EclipsePlatformFake;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class EclipseUserAgentTest {

	@Test
	public void testLanguage() {
		String userAgent = new EclipsePlatformFake(EclipsePlatformFake.LOCALE_US).toString();
		assertLanguage("en-US", userAgent);
	}

	@Test
	public void testApplicationNameAndVersion() {
		String applicationName = "com.jboss.jbds.product";
		String applicationVersion = "3.0.1";
		String userAgent = new EclipsePlatformFake().toString();
		assertApplicationNameAndVersion(applicationName, applicationVersion, userAgent);
	}

	@Test
	public void shouldReturnMacOsSnowLeopard() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_MACOSX, 
				EclipsePlatformFake.VERSION_MACSNOWLEOPARD).toString();
		assertOS(EclipsePlatformFake.MACINTOSH_NAME, 
				EclipsePlatformFake.INTELMACOSX_NAME + EclipsePlatformFake.VERSION_MACSNOWLEOPARD, 
				userAgent);
	}

	@Test
	public void shouldReportLinux32() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_LINUX, 
				EclipsePlatformFake.VERSION_LINUX_FEDORA13,
				EclipsePlatformFake.PROP_SUN_ARCH_32).toString();
		
		assertOS(EclipsePlatformFake.X11_NAME, 
				EclipsePlatformFake.LINUX_NAME + " " + EclipsePlatformFake.LINUX_ARCH_32, 
				userAgent);
	}

	@Test
	public void shouldReportLinux64() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_LINUX, 
				EclipsePlatformFake.VERSION_LINUX_FEDORA13,
				EclipsePlatformFake.PROP_SUN_ARCH_64).toString();
		assertOS(EclipsePlatformFake.X11_NAME, 
				EclipsePlatformFake.LINUX_NAME + " " + EclipsePlatformFake.LINUX_ARCH_64, 
				userAgent);
	}

	@Test
	public void testWindows7_32() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_WIN32, 
				EclipsePlatformFake.VERSION_WIN7,
				EclipsePlatformFake.PROP_SUN_ARCH_32).toString();
		assertOS(EclipsePlatformFake.WINDOWS_NAME, 
				EclipsePlatformFake.WINNT_NAME + " " + EclipsePlatformFake.VERSION_WIN7, 
				userAgent);
	}

	@Test
	public void testWindows7_64() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_WIN32, 
				EclipsePlatformFake.VERSION_WIN7,
				EclipsePlatformFake.PROP_SUN_ARCH_64).toString();
		assertOS(EclipsePlatformFake.WINDOWS_NAME, 
				EclipsePlatformFake.WINNT_NAME + " " + EclipsePlatformFake.VERSION_WIN7 + "; " + EclipsePlatformFake.WINDOWS_ARCH_64, 
				userAgent);
	}

	@Test
	public void testWindows8_32() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_WIN32, 
				EclipsePlatformFake.VERSION_WIN8,
				EclipsePlatformFake.PROP_SUN_ARCH_32).toString();
		assertOS(EclipsePlatformFake.WINDOWS_NAME, 
				EclipsePlatformFake.WINNT_NAME + " " + EclipsePlatformFake.VERSION_WIN8,
				userAgent);
	}

	@Test
	public void testWindows8_64() {
		String userAgent = new EclipsePlatformFake(
				EclipsePlatformFake.LOCALE_US, 
				Platform.OS_WIN32, 
				EclipsePlatformFake.VERSION_WIN8,
				EclipsePlatformFake.PROP_SUN_ARCH_64).toString();
		assertOS(EclipsePlatformFake.WINDOWS_NAME, 
				EclipsePlatformFake.WINNT_NAME + " " + EclipsePlatformFake.VERSION_WIN8 + "; " + EclipsePlatformFake.WINDOWS_ARCH_64,
				userAgent);
	}

	private void assertApplicationNameAndVersion(String applicationName, String applicationVersion, String userAgent) {
		Matcher matcher = Pattern.compile("([a-zA-Z\\.]+)/([0-9\\.]+).+").matcher(userAgent);
		assertTrue(matcher.matches());
		assertEquals(2, matcher.groupCount());
		assertEquals(applicationName, matcher.group(1));
		assertEquals(applicationVersion, matcher.group(2));
	}

	private void assertOS(String platform, String os, String userAgent) {
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
