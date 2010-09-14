package org.jboss.tools.usage.test;

import static org.junit.Assert.assertThat;

import java.util.Collection;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.reporting.JBossComponents;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.osgi.framework.Bundle;

public class JBossComponentsTest {

	@Test
	public void reportedComponentsListIsComplete() {
		Collection<String> componentIds = JBossComponents.getComponentIds(getBundleGroupProviders());

		assertThat(componentIds, JUnitMatchers.hasItems(
				JBossComponents.JBossToolsFeatureNames.GWT.name(),
				JBossComponents.JBossToolsFeatureNames.SEAM.name(),
				JBossComponents.JBossToolsFeatureNames.SMOOKS.name()));
	}

	protected IBundleGroupProvider[] getBundleGroupProviders() {
		return new IBundleGroupProvider[] { new IBundleGroupProvider() {

			public String getName() {
				return "bundleGroupProviderFake";
			}

			public IBundleGroup[] getBundleGroups() {
				return new IBundleGroup[] {
						new BundleGroupFake("org.jboss.tools.usage")			
				}; 
			}
		} 
		};
	}

	
	private class BundleGroupFake implements IBundleGroup {
		private String name;

		private BundleGroupFake(String name) {
			this.name = name;
		}

		public String getIdentifier() {
			throw new UnsupportedOperationException();
		}

		public String getName() {
			return name;
		}

		public String getVersion() {
			throw new UnsupportedOperationException();
		}

		public String getDescription() {
			throw new UnsupportedOperationException();
		}

		public String getProviderName() {
			throw new UnsupportedOperationException();
		}

		public Bundle[] getBundles() {
			throw new UnsupportedOperationException();
		}

		public String getProperty(String key) {
			throw new UnsupportedOperationException();
		}
	}
}
