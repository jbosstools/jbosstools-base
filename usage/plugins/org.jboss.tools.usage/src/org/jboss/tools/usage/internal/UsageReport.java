/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.ITracker;
import org.jboss.tools.usage.googleanalytics.FocusPoint;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.googleanalytics.ILoggingAdapter;
import org.jboss.tools.usage.googleanalytics.Tracker;
import org.jboss.tools.usage.preferences.IUsageReportPreferenceConstants;
import org.jboss.tools.usage.util.StatusUtils;
import org.osgi.service.prefs.BackingStoreException;

public class UsageReport {

	private static final String GANALYTICS_TRACKINGCODE = "UA-17645367-1";

	private static final String HOST_NAME = "jboss.org";

	private FocusPoint focusPoint = new FocusPoint("jboss.org")
			.setChild(new FocusPoint("tools").setChild(new FocusPoint("usage").setChild(new FocusPoint("action")
					.setChild(new FocusPoint("wsstartup")))));

	public void report() {

		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				UsageReportEnablementDialog dialog = new UsageReportEnablementDialog("Report usage",
						"Please allow Red Hat Inc. to report this installation anynomously for statistical matters.",
						"Report usage anonymously to Red Hat Inc.", true, PlatformUI.getWorkbench()
								.getActiveWorkbenchWindow());
				if (isAskUser()) {
					if (dialog.open() == Window.OK) {
						setReportEnabled(dialog.isReportEnabled());
						setAskUser(false);
					}
				}

				if (isReportEnabled()) {
					report(getAnalyticsTracker());
				}
			}
		});
	}

	private void setReportEnabled(boolean enabled) {
		IEclipsePreferences preferences = new ConfigurationScope().getNode(JBossToolsUsageActivator.PLUGIN_ID);
		preferences.putBoolean(IUsageReportPreferenceConstants.USAGEREPORT_ENABLED, enabled);
	}

	private void report(ITracker tracker) {
		tracker.trackAsynchronously(focusPoint);
	}

	private ITracker getAnalyticsTracker() {
		IGoogleAnalyticsParameters eclipseSettings = new EclipseEnvironment(
				GANALYTICS_TRACKINGCODE
				, HOST_NAME
				, IGoogleAnalyticsParameters.VALUE_NO_REFERRAL);
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageActivator.getDefault());
		Tracker tracker = new Tracker(eclipseSettings, loggingAdapter);
		return tracker;
	}

	private boolean isAskUser() {
		IEclipsePreferences preferences = getPreferences();
		return preferences.getBoolean(IUsageReportPreferenceConstants.ASK_USER, true);
	}

	private void setAskUser(boolean askUser) {
		try {
			IEclipsePreferences preferences = getPreferences();
			preferences.putBoolean(IUsageReportPreferenceConstants.ASK_USER, askUser);
			preferences.flush();
		} catch (BackingStoreException e) {
			JBossToolsUsageActivator.getDefault().getLog().log(
					StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID, "Could not save preferences {0}", e,
							IUsageReportPreferenceConstants.ASK_USER));
		}
	}

	private boolean isReportEnabled() {
		return getPreferences().getBoolean(IUsageReportPreferenceConstants.USAGEREPORT_ENABLED, true);
	}

	private IEclipsePreferences getPreferences() {
		return new ConfigurationScope().getNode(JBossToolsUsageActivator.PLUGIN_ID);
	}
}
