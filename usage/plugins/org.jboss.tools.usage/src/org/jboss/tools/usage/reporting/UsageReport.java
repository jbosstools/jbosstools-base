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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.HttpGetRequest;
import org.jboss.tools.usage.IHttpGetRequest;
import org.jboss.tools.usage.ILoggingAdapter;
import org.jboss.tools.usage.ITracker;
import org.jboss.tools.usage.IURLBuildingStrategy;
import org.jboss.tools.usage.PluginLogger;
import org.jboss.tools.usage.Tracker;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.preferences.GlobalUsageReportingSettings;
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

	private GlobalUsageReportingSettings globalSettings = new GlobalUsageReportingSettings(JBossToolsUsageActivator
			.getDefault());

	public void report() {
		new ReportingJob().schedule();
	}

	private void askUser() {
		UsageReportEnablementDialog dialog = new UsageReportEnablementDialog(
				true,
				PlatformUI.getWorkbench().getActiveWorkbenchWindow());
		if (dialog.open() == Window.OK) {
			UsageReportPreferences.setEnabled(dialog.isReportEnabled());
			UsageReportPreferences.setAskUser(false);
			flushPreferences();
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

	private void doReport() {
		if (UsageReportPreferences.isEnabled()) {
			getTracker().trackAsynchronously(focusPoint);
		}
	}

	private ITracker getTracker() {
		IGoogleAnalyticsParameters eclipseEnvironment = new ReportingEclipseEnvironment(
				GANALYTICS_ACCOUNTNAME
				, HOST_NAME
				, PreferencesUtils.getPreferences());
		ILoggingAdapter loggingAdapter = new PluginLogger(JBossToolsUsageActivator.getDefault());
		IURLBuildingStrategy urlStrategy = new GoogleAnalyticsUrlStrategy(eclipseEnvironment);
		IHttpGetRequest httpGetRequest = new HttpGetRequest(eclipseEnvironment.getUserAgent(), loggingAdapter);
		return new Tracker(urlStrategy, httpGetRequest, loggingAdapter);
	}

	private class ReportingJob extends Job {
		private ReportingJob() {
			super(ReportingMessages.UsageReport_Reporting_Usage);
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			monitor.beginTask(ReportingMessages.UsageReport_Querying_Enablement, 2);
			if (globalSettings.isEnabled()) {
				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
				monitor.worked(1);
				if (UsageReportPreferences.isAskUser()) {
					if (monitor.isCanceled()) {
						return Status.CANCEL_STATUS;
					}
					askUserAndReport();
				} else {
					if (monitor.isCanceled()) {
						return Status.CANCEL_STATUS;
					}
					doReport();
				}
				monitor.worked(2);
				monitor.done();
			}
			return Status.OK_STATUS;
		}

		private void askUserAndReport() {
			Job askUserJob = new AskUserJob();
			askUserJob.addJobChangeListener(new IJobChangeListener() {

				public void sleeping(IJobChangeEvent event) {
					// ignore
				}

				public void scheduled(IJobChangeEvent event) {
					// ignore
				}

				public void running(IJobChangeEvent event) {
					// ignore
				}

				public void done(IJobChangeEvent event) {
					doReport();
				}

				public void awake(IJobChangeEvent event) {
					// ignore
				}

				public void aboutToRun(IJobChangeEvent event) {
					// ignore
				}
			});
			askUserJob.setUser(true);
			askUserJob.setPriority(Job.SHORT);
			askUserJob.schedule();
		}
	}

	private class AskUserJob extends UIJob {
		private AskUserJob() {
			super(ReportingMessages.UsageReport_Asking_User);
		}

		@Override
		public IStatus runInUIThread(IProgressMonitor monitor) {
			askUser();
			return Status.OK_STATUS;
		}
	}
}
