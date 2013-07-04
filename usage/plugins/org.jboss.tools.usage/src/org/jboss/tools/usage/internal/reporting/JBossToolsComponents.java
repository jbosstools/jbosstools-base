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
package org.jboss.tools.usage.internal.reporting;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.util.collectionfilter.CollectionFilterUtils;
import org.jboss.tools.usage.util.collectionfilter.ICollectionFilter;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsComponents {

	/**
	 * The jboss tools features to check and report.
	 * <p>
	 * ATTENTION: the following features do not start with org.jboss.tools
	 * </p>
	 * <p>
	 * <ul>
	 * <li>org.hibernate.eclipse.feature</li>
	 * <li>org.jboss.ide.eclipse.freemarker.feature</li>
	 * <li>org.drools.eclipse.feature</li>
	 * <li>org.jboss.ide.eclipse.as.feature</li>
	 * <li>org.mozilla.xulrunner.feature</li>
	 * </ul>
	 * </p>
	 * 
	 */
	public enum JBossToolsFeatureIdentifiers {
		ARQUILLIAN("org.jboss.tools.arquillian.feature"),
		AS("org.jboss.ide.eclipse.as.feature"),
		ARCHIVES("org.jboss.ide.eclipse.archives.feature"),
		BIRT("org.jboss.tools.birt.feature"),
		BPEL("org.jboss.tools.bpel.feature"),
		CDI("org.jboss.tools.cdi.feature"),
		CENTRAL("org.jboss.tools.central.feature"),
		COMMON("org.jboss.tools.common.feature"),
		DELTACLOUD("org.jboss.tools.deltacloud.feature"),
		DROOLS("org.drools.eclipse.feature"),
		ESB("org.jboss.tools.esb.feature"),
		EXAMPLES("org.jboss.tools.project.examples.feature"),
		FORGE("org.jboss.tools.forge.feature"),
		FLOW("org.jboss.tools.flow.common.feature"),
		FREEMARKER("org.jboss.ide.eclipse.freemarker.feature"),
		GWT("org.jboss.tools.gwt.feature"),
		HIBERNATETOOLS("org.hibernate.eclipse.feature"),
		JBPM("org.jboss.tools.jbpm.common.feature"),
		JMX("org.jboss.tools.jmx.feature"),
		JSF("org.jboss.tools.jsf.feature"),
		MAVEN("org.jboss.tools.maven.feature"),
		MODESHAPE("org.jboss.tools.modeshape.rest.feature"),
		OPENSHIFT("org.jboss.tools.openshift.express.feature"),
		PORTLET("org.jboss.tools.portlet.feature"),
		PROFILER("org.jboss.tools.profiler.feature"),
		RUNTIME("org.jboss.tools.runtime.feature"),
		SEAM("org.jboss.tools.seam.feature"),
		SMOOKS("org.jboss.tools.smooks.feature"),
		STRUTS("org.jboss.tools.struts.feature"),
		SWTICHYARD("org.switchyard.tools.feature"),
		USAGE("org.jboss.tools.usage.feature"),
		VPE("org.jboss.tools.vpe.feature"),
		WORKINGSET("org.jboss.tools.workingset.feature"),
		// includes jax-rs
		WS("org.jboss.tools.ws.feature"),
		XULRUNNER("org.mozilla.xulrunner.feature");

		private String featureIdentifier;

		JBossToolsFeatureIdentifiers(String featureIdentifier) {
			this.featureIdentifier = featureIdentifier;
		}

		/**
		 * Returns whether the given bundle group has the same name as the this
		 * feature name.
		 * 
		 * @param bundleName
		 *            the bundle name to check whether it's a member of this
		 *            group of bundles.
		 * @return <tt>true</tt>, if the given bundle
		 */
		public boolean matches(IBundleGroup bundleGroup) {
			return featureIdentifier.equals(bundleGroup.getIdentifier());
		}

		public String getComponentName() {
			return name();
		}

		public String getFeatureName() {
			return featureIdentifier;
		}
	}

	public enum JBossToolsBundleSymbolicName {
		AEROGEAR("org.jboss.tools.aerogear.hybrid.core"),
		LIVERELOAD("org.jboss.tools.livereload.core");

		private String symbolicName;

		private JBossToolsBundleSymbolicName(String symbolicName) {
			this.symbolicName = symbolicName;
		}

		public String getComponentName() {
			return name();
		}

		public String getSymbolicName() {
			return symbolicName;
		}
	}
	
	private JBossToolsComponents() {
		// inhibit instantiation
	}

	/**
	 * Returns the jboss components that the given bundle group provider
	 * provides
	 * 
	 * @param bundleGroupProviders
	 *            the bundles group providers to check for jboss components
	 * @param bundleProvider
	 *            an implementation that allows you to query for a bundle
	 *            whether it's installed
	 * 
	 * @return
	 */
	public static Collection<String> getComponentIds(IBundleGroupProvider[] bundleGroupProviders, IBundleProvider bundleProvider) {
		Set<String> componentNames = new TreeSet<String>();
		for (IBundleGroupProvider bundleGroupProvider : bundleGroupProviders) {
			CollectionFilterUtils.filter(
					new JBossToolsFeaturesFilter(componentNames)
					, bundleGroupProvider.getBundleGroups(), null);
		}

		componentNames.addAll(getInstalledJBossBundles(bundleProvider));
		return componentNames;
	}

	private static Collection<String> getInstalledJBossBundles(IBundleProvider bundleQuery) {
		List<String> installedJBossBundles = new ArrayList<String>();
		for (JBossToolsBundleSymbolicName jbossBundle : JBossToolsBundleSymbolicName.values()) {
			if (bundleQuery.isInstalled(jbossBundle.getSymbolicName())) {
				installedJBossBundles.add(jbossBundle.getComponentName());
			}
		}
		return installedJBossBundles;
	}

	private static class JBossToolsFeaturesFilter implements ICollectionFilter<IBundleGroup> {

		private Collection<String> componentNames;

		private JBossToolsFeaturesFilter(Collection<String> componentNames) {
			this.componentNames = componentNames;
		}

		public boolean matches(IBundleGroup bundleGroup) {
			for (JBossToolsFeatureIdentifiers featureIdentifier : JBossToolsFeatureIdentifiers.values()) {
				if (featureIdentifier.matches(bundleGroup)) {
					this.componentNames.add(featureIdentifier.getComponentName());
					return true;
				}
			}
			return false;
		}
	}

	/**
	 * An interface for classes that allow one to query for installable bundle(s).
	 */
	public interface IBundleProvider {
		public boolean isInstalled(String symbolicName);
	}
}