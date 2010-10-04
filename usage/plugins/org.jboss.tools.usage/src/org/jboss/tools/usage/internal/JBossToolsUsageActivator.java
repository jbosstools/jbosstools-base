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
package org.jboss.tools.usage.internal;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.branding.IUsageBranding;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.branding.JBossToolsUsageBranding;
import org.jboss.tools.usage.internal.branding.UsageBrandingMediator;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferencesUtils;
import org.jboss.tools.usage.internal.reporting.JBossToolsEclipseEnvironment;
import org.osgi.framework.BundleContext;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageActivator extends Plugin {

	public static final String PLUGIN_ID = "org.jboss.tools.usage"; //$NON-NLS-1$

	private static JBossToolsUsageActivator plugin;

	private IJBossToolsEclipseEnvironment eclipseEnvironment;

	private UsageBrandingMediator branding;

	public JBossToolsUsageActivator() {
		plugin = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		branding.close();
		this.branding = null;
		super.stop(context);
	}

	public static JBossToolsUsageActivator getDefault() {
		return plugin;
	}

	public void start(BundleContext context) throws Exception {
		super.start(context);
		initBranding(context);
	}

	private void initBranding(BundleContext context) {
		branding = new UsageBrandingMediator(
				new JBossToolsUsageBranding()
				, getBundle().getBundleContext());
		branding.open();
	}

	public IJBossToolsEclipseEnvironment getJBossToolsEclipseEnvironment() {
		if (eclipseEnvironment == null) {
			eclipseEnvironment = createEclipseEnvironment();
		}
		return eclipseEnvironment;
	}

	private IJBossToolsEclipseEnvironment createEclipseEnvironment() {
		IUsageBranding branding = getUsageBranding();
		return new JBossToolsEclipseEnvironment(
				branding.getGoogleAnalyticsAccount(), branding.getGoogleAnalyticsReportingHost(),
				UsageReportPreferencesUtils.getPreferences());
	}

	public synchronized IUsageBranding getUsageBranding() {
		return branding;
	}
}
