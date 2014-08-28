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

import static org.junit.Assert.assertThat;

import java.util.Collection;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.hamcrest.CoreMatchers;
import org.jboss.tools.usage.internal.reporting.JBossToolsComponents;
import org.jboss.tools.usage.internal.reporting.JBossToolsComponents.IBundleProvider;
import org.jboss.tools.usage.test.fakes.BundleGroupProviderFake;
import org.jboss.tools.usage.test.fakes.EclipseBundleProviderFake;
import org.junit.Test;

public class JBossToolsComponentsTest {

	@Test
	public void reportedComponentsListIsComplete() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getFeatureId()),
						new BundleGroupProviderFake(
								JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getFeatureId(), "rubbish"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.SMOOKS.getFeatureId()),
						new BundleGroupProviderFake("org.jboss.tools.usage.feature.badname")
					}, 
				new NoBundlesProvider());

		assertThat(componentIds, CoreMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SMOOKS.getComponentName()));
	}

	@Test
	public void reportsAS() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getFeatureId())
						, new BundleGroupProviderFake(
								"rubbish",
								JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getFeatureId())
						, new BundleGroupProviderFake(
								"org.jboss.tools.as.feature.badname")
						, new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getFeatureId())
						, new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname")
					}, 
				new NoBundlesProvider());

		assertThat(componentIds, CoreMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getComponentName()));
	}

	@Test
	public void reportsAllFeaturesThatDoNotStartWith_org_jboss_tools() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.GWT.getFeatureId()), 
						new BundleGroupProviderFake(
								"rubbish",
								JBossToolsComponents.JBossToolsFeatureIdentifiers.SEAM.getFeatureId()),
						new BundleGroupProviderFake("org.jboss.tools.as.feature.badname"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getFeatureId()),
						new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname"),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.HIBERNATETOOLS.getFeatureId()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.DROOLS.getFeatureId()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.FREEMARKER.getFeatureId()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.XULRUNNER.getFeatureId()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.FUSE.getFeatureId()),
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.SY.getFeatureId())
					}, 
				new NoBundlesProvider());

		assertThat(componentIds, CoreMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getComponentName()));
		assertThat(componentIds, CoreMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.FUSE.getComponentName()));
		assertThat(componentIds, CoreMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.SY.getComponentName()));
	}
	
	@Test
	public void shouldAddBundles() {
		Collection<String> componentIds = JBossToolsComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getFeatureId())
					}, 
				new EclipseBundleProviderFake(JBossToolsComponents.JBossToolsFeatureIdentifiers.AEROGEAR.getPluginId()));

		assertThat(componentIds, CoreMatchers.hasItems(
				JBossToolsComponents.JBossToolsFeatureIdentifiers.AS.getComponentName(),
				JBossToolsComponents.JBossToolsFeatureIdentifiers.AEROGEAR.getComponentName()));
	}

	private class NoBundlesProvider implements IBundleProvider {

		@Override
		public boolean isInstalled(String symbolicName) {
			return false;
		}
	}
}
