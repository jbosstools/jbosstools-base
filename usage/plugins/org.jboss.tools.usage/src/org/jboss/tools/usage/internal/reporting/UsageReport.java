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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.usage.googleanalytics.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.http.HttpGetRequest;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferences;
import org.jboss.tools.usage.tracker.IFocusPoint;
import org.jboss.tools.usage.tracker.ITracker;
import org.jboss.tools.usage.tracker.IURLBuildingStrategy;
import org.jboss.tools.usage.tracker.internal.FocusPoint;
import org.jboss.tools.usage.tracker.internal.SuffixFocusPoint;
import org.jboss.tools.usage.tracker.internal.Tracker;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Andre Dietisheim
 */
public class UsageReport {

	private IFocusPoint focusPoint;

	private GlobalUsageSettings globalSettings;

	private IJBossToolsEclipseEnvironment eclipseEnvironment;

	UsagePluginLogger logger = new UsagePluginLogger(JBossToolsUsageActivator.getDefault());
	
	public UsageReport() throws InvalidSyntaxException {
		eclipseEnvironment = JBossToolsUsageActivator.getDefault().getJBossToolsEclipseEnvironment();
		focusPoint = new SuffixFocusPoint("tools", eclipseEnvironment.getJBossToolsVersion()) //$NON-NLS-1$ 
				.setChild(new FocusPoint("usage") //$NON-NLS-1$ 
						.setChild(new FocusPoint("action") //$NON-NLS-1$ 
								.setChild(new FocusPoint("wsstartup")))); //$NON-NLS-1$
		globalSettings = new GlobalUsageSettings(JBossToolsUsageActivator
				.getDefault());
	}

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
			logger.error(ReportingMessages.UsageReport_Error_SavePreferences);
		}
	}

	/**
	 * Reports the usage of the current JBoss Tools / JBoss Developer Studio
	 * installation.
	 */
	private void doReport() {
		if (UsageReportPreferences.isEnabled()) {
			IURLBuildingStrategy urlBuildingStrategy = new GoogleAnalyticsUrlStrategy(eclipseEnvironment);
			ITracker tracker = new Tracker(
					urlBuildingStrategy
					, new HttpGetRequest(eclipseEnvironment.getUserAgent(), logger)
					, logger);
			tracker.trackAsynchronously(focusPoint);
		}
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
			if (globalSettings.isReportingEnabled()) {
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
