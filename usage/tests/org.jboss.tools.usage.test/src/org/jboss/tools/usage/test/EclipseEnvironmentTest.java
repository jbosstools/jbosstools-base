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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.internal.reporting.JBossToolsComponents;
import org.jboss.tools.usage.test.fakes.BundleGroupProviderFake;
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

		Matcher matcher = Pattern.compile("(([A-Z]+)-){3}").matcher(keyword);
		assertTrue(matcher.matches());
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


}
