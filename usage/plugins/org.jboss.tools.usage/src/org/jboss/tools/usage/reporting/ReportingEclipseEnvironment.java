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

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class ReportingEclipseEnvironment extends AbstractEclipseEnvironment {

	private static final char BUNDLE_GROUP_DELIMITER = '-';

	public ReportingEclipseEnvironment(String accountName, String hostName, IEclipsePreferences preferences) {
		super(accountName, hostName, preferences);
	}

	@Override
	public String getKeyword() {
		return bundleGroupsToKeywordString(JBossComponents.getComponentIds(getBundles()));
	}

	protected Bundle[] getBundles() {
		return JBossToolsUsageActivator.getDefault().getBundle().getBundleContext().getBundles();
	}

	private String bundleGroupsToKeywordString(Collection<String> jbossComponentNames) {
		char delimiter = BUNDLE_GROUP_DELIMITER;
		StringBuilder builder = new StringBuilder();
		for (String componentName : jbossComponentNames) {
			builder.append(componentName)
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
