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
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.GWT.getFeatureName()),
						new BundleGroupProviderFake(
								JBossToolsComponents.JBossToolsFeatureNames.SEAM.getFeatureName(), "rubbish"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.SMOOKS.getFeatureName()),
						new BundleGroupProviderFake("org.jboss.tools.usage.feature.badname")
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureNames.GWT.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureNames.SEAM.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureNames.SMOOKS.getComponentName()));
		assertFalse(componentIds.contains(JBossToolsComponents.JBossToolsFeatureNames.USAGE.getComponentName()));
	}

	@Test
	public void reportsAS() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.GWT.getFeatureName())
						, new BundleGroupProviderFake(
								"rubbish",
								JBossToolsComponents.JBossToolsFeatureNames.SEAM.getFeatureName())
						, new BundleGroupProviderFake(
								"org.jboss.tools.as.feature.badname")
						, new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.AS.getFeatureName())
						, new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname")
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureNames.AS.getComponentName()));
	}

	@Test
	public void reportsAllFeaturesThatDoNotStartWith_org_jboss_tools() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.GWT.getFeatureName()), 
						new BundleGroupProviderFake(
								"rubbish",
								JBossToolsComponents.JBossToolsFeatureNames.SEAM.getFeatureName()),
						new BundleGroupProviderFake("org.jboss.tools.as.feature.badname"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.AS.getFeatureName()),
						new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.HIBERNATETOOLS.getFeatureName()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.DROOLS.getFeatureName()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.FREEMARKER.getFeatureName()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureNames.XULRUNNER.getFeatureName())
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureNames.AS.getComponentName()));
	}
}
