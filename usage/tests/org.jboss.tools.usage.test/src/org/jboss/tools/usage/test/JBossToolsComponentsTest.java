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
import static org.junit.Assert.assertThat;

import java.util.Collection;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.reporting.JBossToolsComponents;
import org.jboss.tools.usage.test.fakes.BundleGroupProviderFake;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;

public class JBossToolsComponentsTest {

	@Test
	public void reportedComponentsListIsComplete() {
		Collection<String> componentIds = JBossToolsComponents
				.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getFeatureName()),
						new BundleGroupProviderFake(
								JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getFeatureName(), "rubbish"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.SMOOKS.getFeatureName()),
						new BundleGroupProviderFake("org.jboss.tools.usage.feature.badname")
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SMOOKS.getComponentName()));
		assertFalse(componentIds.contains(JBossToolsComponents.JBossToolsFeatureIdentifiers.USAGE.getComponentName()));
	}

	@Test
	public void reportsAS() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getFeatureName())
						, new BundleGroupProviderFake(
								"rubbish",
								JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getFeatureName())
						, new BundleGroupProviderFake(
								"org.jboss.tools.as.feature.badname")
						, new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getFeatureName())
						, new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname")
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getComponentName()));
	}

	@Test
	public void reportsAllFeaturesThatDoNotStartWith_org_jboss_tools() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getFeatureName()), 
						new BundleGroupProviderFake(
								"rubbish",
								JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getFeatureName()),
						new BundleGroupProviderFake("org.jboss.tools.as.feature.badname"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getFeatureName()),
						new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.HIBERNATETOOLS.getFeatureName()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.DROOLS.getFeatureName()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.FREEMARKER.getFeatureName()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.XULRUNNER.getFeatureName())
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getComponentName()));
	}
}
