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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.DateUtils;
import org.jboss.tools.usage.util.StringUtils;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsEclipseEnvironment extends AbstractEclipseEnvironment implements IJBossToolsEclipseEnvironment {

	private static final char JBOSS_COMPONENTS_DELIMITER = '-';

	private static final DateFormat DATE_FORMAT = SimpleDateFormat.getDateTimeInstance(DateFormat.MEDIUM,
			DateFormat.SHORT);

	public JBossToolsEclipseEnvironment(String accountName, String hostName, IEclipsePreferences preferences) {
		super(accountName, hostName, preferences);
	}

	@Override
	public String getKeyword() {
		Collection<String> jbossComponentNames = JBossToolsComponents.getComponentIds(getBundleGroupProviders());
		return bundleGroupsToKeywordString(jbossComponentNames);
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

	public String getJBossToolsVersion() {
		return JBossToolsUsageActivator.getDefault().getBundle().getVersion().toString();
	}

	@Override
	public String toHumanReadable() {
		StringBuilder builder = new StringBuilder();

		builder.append("JBoss Tools Version: ")
				.append(getJBossToolsVersion())
				.append(StringUtils.getLineSeparator());

		builder.append("JBoss Tools Components: ")
				.append(getKeyword())
				.append(StringUtils.getLineSeparator());

		builder.append("Number of Visits: ")
				.append(getVisitCount())
				.append(StringUtils.getLineSeparator());

		builder.append("First Visit: ")
				.append(getFormattedDate(getFirstVisit()))
				.append(StringUtils.getLineSeparator());

		builder.append("Last Visit: ")
				.append(getFormattedDate(getLastVisit()))
				.append(StringUtils.getLineSeparator());

		builder.append("Google Analytics Account: ")
				.append(getAccountName())
				.append(StringUtils.getLineSeparator());
		return builder.toString();
	}

	private String getFormattedDate(String dateString) {
		Date date = DateUtils.checkedParseDateString(dateString);
		if (date != null) {
			return DATE_FORMAT.format(date);
		} else {
			return "";
		}
	}
}
