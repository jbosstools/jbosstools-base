/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 *     Zend Technologies Ltd. - JBIDE-18678
 ******************************************************************************/
package org.jboss.tools.usage.internal.event;

import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.event.IUsageReporterService;
import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.event.UsageReporter;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.googleanalytics.RequestType;
import org.jboss.tools.usage.impl.JBossToolsUsageImplActivator;
import org.jboss.tools.usage.internal.event.EventRegister.Result;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferences;
import org.jboss.tools.usage.internal.reporting.ReportingMessages;
import org.jboss.tools.usage.internal.reporting.UsageReportEnablementDialog;
import org.jboss.tools.usage.internal.reporting.UsageRequest;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Alexey Kazakov
 * @author Andre Dietisheim
 * @author Kaloyan Raev
 */
public class UsageReporterService implements IUsageReporterService {

	private static final String PATH_PREFIX = "/tools/";

	private UsageRequest usageRequest;
	private Lock lockToAskUser = new ReentrantLock();
	private GlobalUsageSettings settings;

	/**
	 * Registers the event type
	 * 
	 * @param type
	 */
	@Override
	public void registerEvent(final UsageEventType type) {
		try {
			getEventRegister().registerEvent(type);
			if (isEnabled()) {
				ReportingJob job = new ReportingJob(new Reporter() {
					@Override
					public void report() {
						checkCountEventInternal(type);
					}
				});
				job.schedule();
			}
		} catch(Exception t) {
			// Catch all Exceptions to make sure, in case of bugs, Usage doesn't prevent JBT from working
			JBossToolsUsageImplActivator.getDefault().getLogger().error(t, true);
		}
	}

	/**
	 * Tracks a user's event 
	 * 
	 * @param event
	 */
	@Override
	public void trackEvent(final UsageEvent event) {
		try {
			if (isEnabled()) {
				ReportingJob job = new ReportingJob(new Reporter() {
					@Override
					public void report() {
						trackEventInternal(event);
					}
				});
				job.schedule();
			}
		} catch(Exception t) {
			// Catch all Exceptions to make sure, in case of bugs, Usage doesn't prevent JBT from working
			JBossToolsUsageImplActivator.getDefault().getLogger().error(t, true);
		}
	}

	/**
	 * Tracks a user's event
	 * 
	 * @param pagePath
	 * @param title
	 * @param event
	 * @param type
	 * @param startNewVisitSession
	 */
	@Override
	public void trackEvent(final String pagePath,
			final String title,
			final UsageEvent event,
			final RequestType type,
			final boolean startNewVisitSession) {
		try {
			if (isEnabled()) {
				ReportingJob job = new ReportingJob(new Reporter() {
					@Override
					public void report() {
						trackEventInternal(pagePath, title, event, type, startNewVisitSession);
					}
				});
				job.schedule();
			}
		} catch(Exception t) {
			// Catch all Exceptions to make sure, in case of bugs, Usage doesn't prevent JBT from working
			JBossToolsUsageImplActivator.getDefault().getLogger().error(t, true);
		}
	}

	/**
	 * Doesn't send a tracking request instantly but remembers the event's value for tracking events once a day.
	 * If the type of this event was used for sending or counting events a day before then a new event with a sum (if bigger than 0) of all previously collected events is sent.
	 * Category, action names and labels are taken into account when values are being counted.
	 * For events without labels and/or values the "N/A" is used as a label and "1" is used as the default value.
	 * @param event  
	 */
	@Override
	public void countEvent(final UsageEvent event) {
		try {
			if (isEnabled()) {
				ReportingJob job = new ReportingJob(new Reporter() {
					@Override
					public void report() {
						countEventInternal(event);
					}
				});
				job.schedule();
			}
		} catch(Exception t) {
			// Catch all Exceptions to make sure, in case of bugs, Usage doesn't prevent JBT from working
			JBossToolsUsageImplActivator.getDefault().getLogger().error(t, true);
		}
	}

	/**
	 * Sends a tracking request for all daily events if it's time to send them 
	 */
	@Override
	public void trackCountEvents() {
		try {
			if (isEnabled()) {
				ReportingJob job = new ReportingJob(new Reporter() {
					@Override
					public void report() {
						trackCountEventsInternal();
					}
				});
				job.schedule();
			}
		} catch(Exception t) {
			// Catch all Exceptions to make sure, in case of bugs, Usage doesn't prevent JBT from working
			JBossToolsUsageImplActivator.getDefault().getLogger().error(t, true);
		}
	}

	private boolean isEnabled() {
		return !UsageReportPreferences.isEnablementSet() || isPreferencesEnabled();
	}

	protected boolean trackEventInternal(String pagePath, String title, UsageEvent event, RequestType type, boolean startNewVisitSession) {
		boolean result = false;
		if (isPreferencesEnabled()) {
			result = sendRequest(pagePath, title, event, type, startNewVisitSession, false);
		}
		return result;
	}

	protected boolean trackEventInternal(UsageEvent event) {
		boolean result = false;
		if (isPreferencesEnabled()) {
			result = sendRequest(getPagePath(event), event.getType().getComponentName(), event, false);
		}
		return result;
	}

	protected boolean countEventInternal(UsageEvent event) {
		boolean result = false;
		if (isPreferencesEnabled()) {
			result = sendRequest(getPagePath(event), event.getType().getComponentName(), event, true);
		}
		return result;
	}

	/**
	 * Checks if the corresponding daily event should be sent. If yes then that daily event(s) is sent. 
	 * @param type
	 * @return the number of sent events
	 */
	protected int checkCountEventInternal(UsageEventType type) {
		int sent = 0;
		if (isPreferencesEnabled()) {
			Set<Result> results = getEventRegister().checkCountEvent(type, getGlobalUsageSettings());
			for (Result result : results) {
				if(result.isOkToSend()) {
					int value = result.getPreviousSumOfValues();
					String label = result.getCountEventLabel();
					UsageEvent event = type.event(label, value);
					if(getUsageRequest().sendRequest(getPagePath(event), event.getType().getComponentName(), event, null, false)) {
						sent++;
					}
				}
			}
		}
		return sent;
	}

	/**
	 * Returns number of sent daily events
	 * @return
	 */
	protected int trackCountEventsInternal() {
		int result = 0;
		UsageEventType[] types = getEventRegister().getRegisteredEventTypes();
		for (UsageEventType type : types) {
			result = result + checkCountEventInternal(type);
		}
		return result;
	}

	/**
	 * Sends a page view tracking request with the given event in the current session
	 */
	private boolean sendRequest(String pagePath, String title, UsageEvent event, boolean onceADayOnly) {
		return sendRequest(pagePath, title, event, null, false, onceADayOnly);
	}

	/**
	 * Sends a tracking request
	 * @param environment
	 * @param pagePath
	 * @param title may be null
	 * @param event may not be null if onceADayOnly==true
	 * @param type if null, RequestType.PAGE is used
	 * @param startNewVisitSession if false, the current session from environment is used
	 * @param onceADayOnly if true, send a request only once a day
	 * @return true if the request was sent successfully
	 */
	synchronized private boolean sendRequest(String pagePath,
			String title,
			UsageEvent event,
			RequestType type,
			boolean startNewVisitSession,
			boolean onceADayOnly) {

		boolean sent = false;
		if(onceADayOnly) {
			event = event.clone();
			if(event.getLabel()==null) {
				event.setLabel(UsageReporter.NOT_APPLICABLE_LABEL);
			}
			if(event.getValue()==null) {
				event.setValue(1);
			}
		}
		EventRegister.Result result = getEventRegister().checkTrackData(event, getGlobalUsageSettings(), onceADayOnly);
		if(result.isOkToSend()) {
			int value = result.getPreviousSumOfValues();
			if(onceADayOnly && value>0) {
				event.setValue(value);
				event.setLabel(result.getCountEventLabel());
			}
			sent = getUsageRequest().sendRequest(pagePath, title, event, type, startNewVisitSession);
		}
		return sent;
	}

	protected EventRegister getEventRegister() {
		return EventRegister.getInstance();
	}

	private String getPagePath(UsageEvent event) {
		return PATH_PREFIX + event.getType().getComponentName() + "/" + event.getType().getComponentVersion();
	}

	protected boolean isPreferencesEnabled() {
		return UsageReportPreferences.isEnabled();
	}

	protected UsageRequest getUsageRequest() {
		if(usageRequest==null) {
			usageRequest = new UsageRequest(getEnvironment());
		}
		return usageRequest;
	}

	protected synchronized GlobalUsageSettings getGlobalUsageSettings() {
		if(settings==null) {
			settings = new GlobalUsageSettings(JBossToolsUsageImplActivator.getDefault());
		}
		return settings;
	}

	protected IJBossToolsEclipseEnvironment getEnvironment() {
		return JBossToolsUsageImplActivator.getDefault().getJBossToolsEclipseEnvironment();
	}

	/**
	 * Checks reporting shall ask the user (which is the case if this was never
	 * done before. Asking user shall only be done once per eclipse
	 * installation)
	 * 
	 * @return true, if the user should be asked
	 */
	protected boolean isAskUser() {
		return UsageReportPreferences.isAskUser();
	}

	/**
	 * Asks the user is he allows us to report usage. Opens a dialog for this sake.
	 */
	private void askUser() {
		Boolean isEnabled = askUserForEnablement();
		if (isEnabled != null) {
			UsageReportPreferences.setEnabled(isEnabled);
			UsageReportPreferences.setAskUser(false);
			try {
				UsageReportPreferences.flush();
			} catch (BackingStoreException e) {
				JBossToolsUsageImplActivator.getDefault().getLogger().error(e);
			}
		}
	}

	/**
	 * Asks the user if he allows us to report.
	 * 
	 * @return the boolean
	 */
	protected Boolean askUserForEnablement() {
		final Boolean[] userResponse = new Boolean[1];
		Display.getDefault().syncExec(new Runnable() {
			@Override
			public void run() {
				Shell shell = PlatformUI.getWorkbench().getModalDialogShellProvider().getShell();
				UsageReportEnablementDialog dialog =
						new UsageReportEnablementDialog(
								shell,
								JBossToolsUsageImplActivator.getDefault().getUsageBranding());
				if (dialog.open() == Window.OK) {
					userResponse[0] = dialog.isReportEnabled();
				} else {
					userResponse[0] = false;
				}
			}
		});
		return userResponse[0];
	}

	private static interface Reporter {
		void report();
	}

	private class ReportingJob extends Job {
		Reporter reporter;
		private ReportingJob(Reporter reporter) {
			super(ReportingMessages.UsageReport_Reporting_Usage);
			this.reporter = reporter;
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			monitor.beginTask(ReportingMessages.UsageReport_Querying_Enablement, 2);
			if (getGlobalUsageSettings().isReportingEnabled()) {
				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
				monitor.worked(1);
				try {
					lockToAskUser.lock();
					if (isAskUser()) {
						if (monitor.isCanceled()) {
							return Status.CANCEL_STATUS;
						}
						askUser();
					}
				} finally {
					lockToAskUser.unlock();
				}
				reporter.report();
				monitor.worked(2);
				monitor.done();
			}
			return Status.OK_STATUS;
		}
	}
}