/*******************************************************************************
 * Copyright (c) 2010-2017 Red Hat, Inc.
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
import static org.junit.Assert.assertNotNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.internal.environment.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.internal.reporting.JBossToolsComponents;
import org.jboss.tools.usage.test.fakes.BundleGroupProviderFake;
import org.jboss.tools.usage.test.fakes.EclipsePreferencesFake;
import org.jboss.tools.usage.test.fakes.ReportingEclipseEnvironmentFake;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class EclipseEnvironmentTest {

	@Test
	public void keywordReportsJbossComponents() {
		AbstractEclipseEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake() {

			@Override
			protected IBundleGroupProvider[] getBundleGroupProviders() {
				return new IBundleGroupProvider[] {
						new BundleGroupProviderFake(
								"org.jboss.tools.gwt.feature",
								"org.jboss.tools.seam.feature",
								"org.jboss.tools.smooks.feature")
				};
			}
		};

		String keyword = eclipseEnvironment.getKeyword();

		Matcher matcher = Pattern.compile("(([A-Z]+)-){2}(([A-Z]+)){1}").matcher(keyword);
		assertTrue(matcher.matches());
		assertEquals(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.name().length() +
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.name().length() +
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SMOOKS.name().length() +
				2, matcher.group().length());
		assertTrue(keyword.indexOf(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.name()) >= 0);
		assertTrue(keyword.indexOf(JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.name()) >= 0);
		assertTrue(keyword.indexOf(JBossToolsComponents.JBossToolsFeatureIdentifiers.SMOOKS.name()) >= 0);
	}

	@Test
	public void keywordIsEmptyIfNoJBossFeaturesFound() {
		AbstractEclipseEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake() {

			@Override
			protected IBundleGroupProvider[] getBundleGroupProviders() {
				return new IBundleGroupProvider[] {
						new BundleGroupProviderFake(
								"org.jboss.tools.gwt",
								"org.eclipse.emf.cdo")
				};
			}
		};

		String keyword = eclipseEnvironment.getKeyword();
		assertTrue(keyword != null && keyword.length() == 0);
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

	@Test
	public void testJVMName() {
		EclipsePreferencesFake preferences = new EclipsePreferencesFake();
		AbstractEclipseEnvironment eclipseEnvironment = new ReportingEclipseEnvironmentFake(preferences);

		String bitVersion = eclipseEnvironment.getJavaBitVersion();
		assertTrue(bitVersion.equals("32")||bitVersion.equals("64")||bitVersion.equals("unknown"));

		String name = eclipseEnvironment.getJavaVmName();
		assertNotNull(name);
		String vendor = eclipseEnvironment.getJavaVendor();
		assertNotNull(vendor);
		String version = eclipseEnvironment.getFlashVersion();
		assertNotNull(version);
	}
}