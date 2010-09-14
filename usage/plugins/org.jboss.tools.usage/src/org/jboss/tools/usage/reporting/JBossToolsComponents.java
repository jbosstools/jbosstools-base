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
package org.jboss.tools.usage.reporting;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.jboss.tools.usage.util.collectionfilter.CollectionFilterUtils;
import org.jboss.tools.usage.util.collectionfilter.CompositeCollectionFilter;
import org.jboss.tools.usage.util.collectionfilter.ICollectionFilter;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsComponents {

	public enum JBossToolsFeatureNames {
		ARCHIVES("org.jboss.ide.eclipse.archives.feature"),
		BIRT("org.jboss.tools.birt.feature"),
		BPEL("org.jboss.tools.bpel.feature"),
		CDI("org.jboss.tools.cdi.feature"),
		COMMON("org.jboss.tools.common.feature"),
		DELTACLOUD("org.jboss.tools.deltacloud.feature"),
		DROOLS("org.drools.eclipse.feature"),
		ESB("org.jboss.tools.esb.feature"),
		EXAMPLES("org.jboss.tools.project.examples.feature"),
		FLOW("org.jboss.tools.flow.common.feature"),
		FREEMARKER("org.jboss.ide.eclipse.freemarker.feature"),
		GWT("org.jboss.tools.gwt.feature"),
		HIBERNATETOOLS("org.hibernate.eclipse.feature"),
		JBPM("org.jboss.tools.jbpm.common.feature"),
		JMX("org.jboss.tools.jmx.feature"),
		JSF("org.jboss.tools.jsf.feature"),
		MAVEN("org.jboss.tools.maven.feature"),
		MODESHAPE("org.jboss.tools.modeshape.rest.feature"),
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

		private String featureName;

		JBossToolsFeatureNames(String featureName) {
			this.featureName = featureName;
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
			return featureName.equals(bundleGroup.getName());
		}

		public String getAbbreviation() {
			return name();
		}
	}

	private static final String JBOSS_TOOLS_BUNDLES_PREFIX = "org\\.jboss\\.tools.+"; //$NON-NLS-1$

	private JBossToolsComponents() {
		// inhibit instantiation
	}


	/**
	 * Returns the jboss components that the given bundle group provider provides
	 * 
	 * @param bundles
	 *            the bundles group providers to check for jboss components
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static Collection<String> getComponentIds(IBundleGroupProvider[] bundleGroupProviders) {
		Set<String> componentNames = new TreeSet<String>();
		for (IBundleGroupProvider bundleGroupProvider : bundleGroupProviders) {
			CollectionFilterUtils.filter(
					new CompositeCollectionFilter<IBundleGroup>(
							new JBossToolsNameFilter()
							, new JBossToolsFeaturesFilter(componentNames))
					, bundleGroupProvider.getBundleGroups(), null);
		}
		return componentNames;
	}

	private static class JBossToolsNameFilter implements ICollectionFilter<IBundleGroup> {

		Pattern pattern = Pattern.compile(JBOSS_TOOLS_BUNDLES_PREFIX);

		public boolean matches(IBundleGroup bundleGroup) {
			return pattern.matcher(bundleGroup.getName()).matches();
		}
	}

	private static class JBossToolsFeaturesFilter implements ICollectionFilter<IBundleGroup> {

		private Collection<String> componentNames;

		private JBossToolsFeaturesFilter(Collection<String> componentNames) {
			this.componentNames = componentNames;
		}

		public boolean matches(IBundleGroup bundleGroup) {
			for (JBossToolsFeatureNames featureName : JBossToolsFeatureNames.values()) {
				if (featureName.matches(bundleGroup)) {
					this.componentNames.add(featureName.getAbbreviation());
					return true;
				}
			}
			return false;
		}
	}
}