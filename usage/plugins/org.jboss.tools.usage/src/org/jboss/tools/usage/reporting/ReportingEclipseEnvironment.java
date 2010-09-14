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

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;

/**
 * @author Andre Dietisheim
 */
public class ReportingEclipseEnvironment extends AbstractEclipseEnvironment {

	private static final char JBOSS_COMPONENTS_DELIMITER = '-';

	public ReportingEclipseEnvironment(String accountName, String hostName, IEclipsePreferences preferences) {
		super(accountName, hostName, preferences);
	}

	@Override
	public String getKeyword() {
		Collection<String> jbossComponentNames = JBossToolsComponents.getComponentIds(getBundleGroupProviders());
		return bundleGroupsToKeywordString(jbossComponentNames );
	}

	protected IBundleGroupProvider[] getBundleGroupProviders() {
		return Platform.getBundleGroupProviders();
	}

	private String bundleGroupsToKeywordString(Collection<String> jbossComponentNames) {
		char delimiter = JBOSS_COMPONENTS_DELIMITER;
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
