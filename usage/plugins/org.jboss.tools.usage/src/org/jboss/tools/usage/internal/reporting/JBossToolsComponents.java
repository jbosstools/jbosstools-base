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

import java.util.Collection;
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
		AS("org.jboss.ide.eclipse.as.feature"),
		ARCHIVES("org.jboss.ide.eclipse.archives.feature"),
		BIRT("org.jboss.tools.birt.feature"),
		BPEL("org.jboss.tools.bpel.feature"),
		CDI("org.jboss.tools.cdi.feature"),
		COMMON("org.jboss.tools.common.feature"),
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
		TPTP("org.jboss.tools.tptp.feature"),
		USAGE("org.jboss.tools.usage.feature"),
		VPE("org.jboss.tools.vpe.feature"),
		WORKINGSET("org.jboss.tools.workingset.feature"),
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

	private JBossToolsComponents() {
		// inhibit instantiation
	}

	/**
	 * Returns the jboss components that the given bundle group provider
	 * provides
	 * 
	 * @param bundles
	 *            the bundles group providers to check for jboss components
	 * @return
	 */
	public static Collection<String> getComponentIds(IBundleGroupProvider[] bundleGroupProviders) {
		Set<String> componentNames = new TreeSet<String>();
		for (IBundleGroupProvider bundleGroupProvider : bundleGroupProviders) {
			CollectionFilterUtils.filter(
					/* not all jboss tools features start with org.jboss.tools. @see https://jira.jboss.org/browse/JBIDE-7082 
					 * 
					new CompositeCollectionFilter<IBundleGroup>(
							new JBossToolsNameFilter()
							, new JBossToolsFeaturesFilter(componentNames)) */
					new JBossToolsFeaturesFilter(componentNames)
					, bundleGroupProvider.getBundleGroups(), null);
		}
		return componentNames;
	}

	/* not all jboss tools features start with org.jboss.tools. @see https://jira.jboss.org/browse/JBIDE-7082 
	 * 
	 private static class JBossToolsNameFilter implements
	 ICollectionFilter<IBundleGroup> {
	
	 private static final String JBOSS_TOOLS_FEATURES_PREFIX = "org\\.jboss.+"; //$NON-NLS-1$
	 Pattern pattern = Pattern.compile(JBOSS_TOOLS_FEATURES_PREFIX);
	
	 public boolean matches(IBundleGroup bundleGroup) {
	 return pattern.matcher(bundleGroup.getName()).matches();
	 }
	 }
	 */

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
}