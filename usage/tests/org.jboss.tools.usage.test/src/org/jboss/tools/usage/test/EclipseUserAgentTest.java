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
import static org.junit.Assert.assertTrue;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.test.fakes.EclipsePreferencesFake;
import org.jboss.tools.usage.test.fakes.EclipseUserAgentFake;
import org.jboss.tools.usage.test.fakes.ReportingEclipseEnvironmentFake;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class EclipseUserAgentTest {

	@Test
	public void testLanguage() {
		String userAgent = new EclipseUserAgentFake(EclipseUserAgentFake.LOCALE_US).toString();
		assertLanguage("en-US", userAgent);
	}

	@Test
	public void testApplicationNameAndVersion() {
		String applicationName = "com.jboss.jbds.product";
		String applicationVersion = "3.0.1";
		String userAgent = new EclipseUserAgentFake().toString();
		assertApplicationNameAndVersion(applicationName, applicationVersion, userAgent);
	}

	@Test
	public void testMacOsSnowLeopard() {
		String userAgent = new EclipseUserAgentFake(EclipseUserAgentFake.LOCALE_US, Platform.OS_MACOSX, EclipseUserAgentFake.MACSNOWLEOPARD_VERSION).toString();
		assertOs("Macintosh", "Intel Mac OS X " + EclipseUserAgentFake.MACSNOWLEOPARD_VERSION, userAgent);
	}

	@Test
	public void testLinux() {
		String userAgent = new EclipseUserAgentFake(EclipseUserAgentFake.LOCALE_US, Platform.OS_LINUX, EclipseUserAgentFake.WIN7_VERSION).toString();
		assertOs("X11", "Linux i686", userAgent);
	}

	@Test
	public void testWindows7() {
		String userAgent = new EclipseUserAgentFake(EclipseUserAgentFake.LOCALE_US, Platform.OS_WIN32, EclipseUserAgentFake.WIN7_VERSION).toString();
		assertOs("Windows", "Windows NT " + EclipseUserAgentFake.WIN7_VERSION, userAgent);
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

	@Test
	public void testVisitsOnFirstVisit() {
		EclipsePreferencesFake preferences = new EclipsePreferencesFake();
		AbstractEclipseEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake(preferences);
		String firstVisit = eclipseEnvironment.getFirstVisit();
		assertEquals(1, eclipseEnvironment.getVisitCount());
		assertEquals(firstVisit, eclipseEnvironment.getLastVisit());
		assertEquals(firstVisit, eclipseEnvironment.getLastVisit());
		assertEquals(firstVisit, eclipseEnvironment.getCurrentVisit());
	}

	@Test
	public void testVisitsOnSecondVisit() throws InterruptedException {
		EclipsePreferencesFake preferences = new EclipsePreferencesFake();
		AbstractEclipseEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake(preferences);
		String firstVisit = eclipseEnvironment.getFirstVisit();
		Thread.sleep(10);
		eclipseEnvironment.visit();

		assertEquals(2, eclipseEnvironment.getVisitCount());
		assertEquals(firstVisit, eclipseEnvironment.getFirstVisit());
		assertEquals(firstVisit, eclipseEnvironment.getLastVisit());
		assertTrue(!firstVisit.equals(eclipseEnvironment.getCurrentVisit()));
	}

	@Test
	public void testVisitsOnThirdVisit() throws InterruptedException {
		EclipsePreferencesFake preferences = new EclipsePreferencesFake();
		AbstractEclipseEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake(preferences);
		String firstVisit = eclipseEnvironment.getFirstVisit();
		Thread.sleep(10);
		eclipseEnvironment.visit();

		String currentVisit = eclipseEnvironment.getCurrentVisit();
		Thread.sleep(10);
		eclipseEnvironment.visit();

		assertEquals(3, eclipseEnvironment.getVisitCount());
		assertEquals(currentVisit, eclipseEnvironment.getLastVisit());
		assertTrue(!firstVisit.equals(eclipseEnvironment.getCurrentVisit()));
	}
}
