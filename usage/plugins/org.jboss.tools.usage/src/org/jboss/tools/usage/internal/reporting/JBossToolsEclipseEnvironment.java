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

import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.googleanalytics.eclipse.AbstractEclipseEnvironment;
import org.jboss.tools.usage.googleanalytics.eclipse.IEclipseUserAgent;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsEclipseEnvironment extends AbstractEclipseEnvironment implements IJBossToolsEclipseEnvironment {

	private static final String NOT_INSTALLED = "N/A";  //$NON-NLS-1$
	private static final String TRUE = "true";  //$NON-NLS-1$
	private static final String FALSE = "false";  //$NON-NLS-1$
	private static final char JBOSS_COMPONENTS_DELIMITER = '-';

	private static final String JBOSS_CENTRAL_PLUGIN_ID = "org.jboss.tools.central"; //$NON-NLS-1$
	private static final String SHOW_JBOSS_CENTRAL_ON_STARTUP = "showJBossCentralOnStartup"; //$NON-NLS-1$
	private static final boolean SHOW_JBOSS_CENTRAL_ON_STARTUP_DEFAULT_VALUE = true;

	public JBossToolsEclipseEnvironment(String accountName, String hostName, IEclipsePreferences preferences) {
		super(accountName, hostName, preferences);
	}

	protected JBossToolsEclipseEnvironment(String accountName, String hostName, IEclipsePreferences preferences,
			IEclipseUserAgent userAgent) {
		super(accountName, hostName, preferences, userAgent);
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

	public boolean isLinuxDistro() {
		return getLinuxDistroNameAndVersion() != null;
	}

	/**
	 * TODO: support multiple events.
	 * most likely doesn't work to send multiple events in a single request: {@link}https://issues.jboss.org/browse/JBDS-2573 
	 */
	public GoogleAnalyticsEvent getEvent() {
		return new GoogleAnalyticsEvent("central", "showOnStartup", getCentralEnabledValue());
	}

	public String getCentralEnabledValue() {
		Bundle bundle = Platform.getBundle(JBOSS_CENTRAL_PLUGIN_ID);
		if (bundle == null) {
			return NOT_INSTALLED;
		}
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(JBOSS_CENTRAL_PLUGIN_ID);
		boolean showOnStartup = prefs.getBoolean(SHOW_JBOSS_CENTRAL_ON_STARTUP,
				SHOW_JBOSS_CENTRAL_ON_STARTUP_DEFAULT_VALUE);
		if (showOnStartup) {
			return TRUE;
		}
		return FALSE;
	}
}
