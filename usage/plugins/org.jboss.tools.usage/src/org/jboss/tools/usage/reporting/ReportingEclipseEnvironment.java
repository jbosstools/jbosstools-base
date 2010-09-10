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

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.BundleUtils;
import org.jboss.tools.usage.util.BundleUtils.IBundleEntryFilter;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class ReportingEclipseEnvironment extends AbstractEclipseEnvironment {

	private static final String JBOSS_TOOLS_BUNDLES_PREFIX = "org\\.jboss\\.tools.+"; //$NON-NLS-1$
	private static final char BUNDLE_GROUP_DELIMITER = '-';

	public ReportingEclipseEnvironment(String accountName, String hostName, IEclipsePreferences preferences) {
		super(accountName, hostName, preferences);
	}

	@Override
	public String getKeyword() {
		JBossBundleGroups jbossBundleGroups = new JBossBundleGroups();
		IBundleEntryFilter jbossToolsFilter = new BundleUtils.BundleSymbolicNameFilter(JBOSS_TOOLS_BUNDLES_PREFIX);
		IBundleEntryFilter compositeFilter = new BundleUtils.CompositeFilter(
				jbossToolsFilter
				, jbossBundleGroups);
		BundleUtils.getBundles(compositeFilter, getBundles());

		return bundleGroupsToKeywordString(jbossBundleGroups);
	}

	protected Bundle[] getBundles() {
		return JBossToolsUsageActivator.getDefault().getBundle().getBundleContext().getBundles();
	}

	private String bundleGroupsToKeywordString(JBossBundleGroups jbossBundleGroups) {
		char delimiter = BUNDLE_GROUP_DELIMITER;
		StringBuilder builder = new StringBuilder();
		for (String bundleGroupId : jbossBundleGroups.getBundleGroupIds()) {
			builder.append(bundleGroupId)
					.append(delimiter);
		}
		return builder.toString();
	}

	public String getAdContent() {
		return getBundleVersion();
	}

	private String getBundleVersion() {
		return JBossToolsUsageActivator.getDefault().getBundle().getVersion().toString();
	}
}
