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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.ILoggingAdapter;
import org.jboss.tools.usage.ITracker;
import org.jboss.tools.usage.IURLBuildingStrategy;
import org.jboss.tools.usage.PluginLogger;
import org.jboss.tools.usage.Tracker;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.PreferencesUtils;
import org.jboss.tools.usage.util.StatusUtils;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Andre Dietisheim
 */
public class UsageReport {

	private static final String GANALYTICS_ACCOUNTNAME = ReportingMessages.UsageReport_GoogleAnalyticsAccount;

	private static final String HOST_NAME = ReportingMessages.UsageReport_HostName;

	private FocusPoint focusPoint = new FocusPoint("tools") //$NON-NLS-1$ 
			.setChild(new FocusPoint("usage") //$NON-NLS-1$ 
					.setChild(new FocusPoint("action") //$NON-NLS-1$ 
							.setChild(new FocusPoint("wsstartup")))); //$NON-NLS-1$

	public void report() {
		UsageReportEnablementDialog dialog = new UsageReportEnablementDialog(ReportingMessages.UsageReport_DialogTitle,
				ReportingMessages.UsageReport_DialogMessage,
				ReportingMessages.UsageReport_Checkbox_Text,
				true,
				PlatformUI.getWorkbench().getActiveWorkbenchWindow());
		if (UsageReportPreferences.isAskUser()) {
			if (dialog.open() == Window.OK) {
				UsageReportPreferences.setEnabled(dialog.isReportEnabled());
				UsageReportPreferences.setAskUser(false);
				flushPreferences();
			}
		}

		if (UsageReportPreferences.isEnabled()) {
			report(getTracker());
		}
	}

	private void flushPreferences() {
		try {
			UsageReportPreferences.flush();
		} catch (BackingStoreException e) {
			IStatus status = StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID,
					ReportingMessages.UsageReport_Error_SavePreferences, e);
			JBossToolsUsageActivator.getDefault().getLog().log(status);
		}
	}

	private void report(ITracker tracker) {
		tracker.trackAsynchronously(focusPoint);
	}

	private ITracker getTracker() {
		IGoogleAnalyticsParameters eclipseSettings = new EclipseEnvironment(
				GANALYTICS_ACCOUNTNAME
				, HOST_NAME
				, IGoogleAnalyticsParameters.VALUE_NO_REFERRAL
				, PreferencesUtils.getPreferences());
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageActivator.getDefault());
		IURLBuildingStrategy urlStrategy = new GoogleAnalyticsUrlStrategy(eclipseSettings);
		return new Tracker(urlStrategy, eclipseSettings.getUserAgent(), loggingAdapter);
	}
}
