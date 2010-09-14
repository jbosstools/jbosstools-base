package org.jboss.tools.usage.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;

import java.util.Collection;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.reporting.JBossComponents;
import org.jboss.tools.usage.test.fakes.BundleGroupProviderFake;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;

public class JBossComponentsTest {

	@Test
	public void reportedComponentsListIsComplete() {
		Collection<String> componentIds = JBossComponents.getComponentIds(
				new IBundleGroupProvider[] {
						new BundleGroupProviderFake("org.jboss.tools.gwt.feature")
						, new BundleGroupProviderFake(
								"rubbish",
								"org.jboss.tools.seam.feature")
						, new BundleGroupProviderFake("org.jboss.tools.smooks.feature")
						, new BundleGroupProviderFake("org.jboss.tools.usage.feature.bandname")
					});

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossComponents.JBossToolsFeatureNames.GWT.getAbbreviation(),
				JBossComponents.JBossToolsFeatureNames.SEAM.getAbbreviation(),
				JBossComponents.JBossToolsFeatureNames.SMOOKS.getAbbreviation()));
		assertFalse(componentIds.contains(JBossComponents.JBossToolsFeatureNames.USAGE.getAbbreviation()));
	}
}
