package org.jboss.tools.usage.test;

import static org.junit.Assert.assertTrue;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.reporting.JBossComponents;
import org.junit.Test;
import org.osgi.framework.Bundle;

public class ReportingEclipseEnvironmentTest {

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
		assertTrue(keyword.indexOf(JBossComponents.JBossToolsFeatureNames.GWT.name()) >= 0);
		assertTrue(keyword.indexOf(JBossComponents.JBossToolsFeatureNames.SEAM.name()) >= 0);
		assertTrue(keyword.indexOf(JBossComponents.JBossToolsFeatureNames.SMOOKS.name()) >= 0);
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

	private static class BundleGroupProviderFake implements IBundleGroupProvider {

		private String[] featureNames;

		public BundleGroupProviderFake(String... featureNames) {
			this.featureNames = featureNames;
		}

		public String getName() {
			throw new UnsupportedOperationException();
		}

		public IBundleGroup[] getBundleGroups() {
			IBundleGroup[] bundleGroups = new IBundleGroup[featureNames.length];
			for (int i = 0; i < featureNames.length; i++) {
				bundleGroups[i] = createBundleGroup(featureNames[i]);
			}
			return bundleGroups;
		}

		private IBundleGroup createBundleGroup(final String name) {
			return new IBundleGroup() {

				public String getVersion() {
					throw new UnsupportedOperationException();
				}

				public String getProviderName() {
					throw new UnsupportedOperationException();
				}

				public String getProperty(String key) {
					throw new UnsupportedOperationException();
				}

				public String getName() {
					return name;
				}

				public String getIdentifier() {
					throw new UnsupportedOperationException();
				}

				public String getDescription() {
					throw new UnsupportedOperationException();

				}

				public Bundle[] getBundles() {
					throw new UnsupportedOperationException();
				}
			};
		}

	}
}
