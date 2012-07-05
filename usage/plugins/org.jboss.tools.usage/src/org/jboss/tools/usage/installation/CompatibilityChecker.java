/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.installation;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.preferences.UsageReportPreferences;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

/**
 * @author Alexey Kazakov
 *
 */
public class CompatibilityChecker {

	private static final String SYSTEM_ENABLED_KEY = "eclipse_version_checking_enabled"; //$NON-NLS-1$

	public void check() {
		new CheckingJob(isCheckingEnabled() && UsageReportPreferences.isEclipseVersionCheckingEnabled()).schedule();
	}

	private boolean isEclipseOutOfDate() {
		return isPluginOutOfDate("org.eclipse.jdt", 3, 7, 2);
	}

	private boolean isWTPOfDate() {
		return isPluginOutOfDate("org.eclipse.jst.j2ee.core", 1, 2, 102);
	}

	private boolean isPluginOutOfDate(String pluginId, int major, int minor, int micro) {
		Bundle plugin = Platform.getBundle(pluginId);
		if(plugin!=null) {
			Version version = plugin.getVersion();
			return version.compareTo(new Version(major, minor, micro)) < 0;
		}
		return false;
	}

	private void askUser() {
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				Shell shell = PlatformUI.getWorkbench().getModalDialogShellProvider().getShell();
				EclipseVersionDialog dialog = new EclipseVersionDialog(shell);
				dialog.open();
			}
		});
	}

	/**
	 * Returns <code>true</code> if checking is enabled in system settings.
	 * 
	 * @return
	 */
	private boolean isCheckingEnabled() {
		return Boolean.valueOf(System.getProperty(SYSTEM_ENABLED_KEY, "true"));
	}

	private class CheckingJob extends Job {
		private boolean showDialog;

		private CheckingJob(boolean showDialog) {
			super(CompatibilityMessages.CheckingJob);
			this.showDialog = showDialog;
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			monitor.beginTask(CompatibilityMessages.CheckingJob, 1);
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			if (isEclipseOutOfDate() || isWTPOfDate()) {
				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
				IStatus status = new Status(IStatus.WARNING, JBossToolsUsageActivator.PLUGIN_ID, CompatibilityMessages.CompatibilityCheckerEclipseLogWarningMessage);
				JBossToolsUsageActivator.getDefault().getLog().log(status);
				if(showDialog) {
					askUser();
				}
			}
			monitor.done();
			return Status.OK_STATUS;
		}
	}
}