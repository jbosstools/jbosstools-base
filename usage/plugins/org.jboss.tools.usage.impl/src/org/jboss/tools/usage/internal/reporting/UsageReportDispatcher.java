/*******************************************************************************
 * Copyright (c) 2010-2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.reporting;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.event.UsageReporter;
import org.jboss.tools.usage.googleanalytics.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.googleanalytics.RequestType;
import org.jboss.tools.usage.impl.JBossToolsUsageImplActivator;
import org.jboss.tools.usage.internal.event.CountEventTimer;

/**
 * @author Andre Dieitsheim
 * @author Alexey Kazakov
 */
public class UsageReportDispatcher implements IStartup {

	@Override
	public void earlyStartup() {
		Display.getDefault().asyncExec(new Runnable() {

			@Override
			public void run() {
				UsageReporter reporter = UsageReporter.getInstance();
				JBossToolsUsageImplActivator plugin = JBossToolsUsageImplActivator.getDefault();
				String version = plugin.getBundle().getVersion().toString();
				IJBossToolsEclipseEnvironment environment = plugin.getJBossToolsEclipseEnvironment();
				UsageEventType type = new UsageEventType("usage", version, "central", "showOnStartup", "true|false|N/A");
				String label = environment.getEvent().getValue();
				reporter.registerEvent(type);
				String title = getUsedJVM(environment);
				reporter.trackEvent("/tools/usage/action/wsstartup/" + version, title, type.event(label), RequestType.PAGE, true);

				type = createFinishWizardType();
				reporter.registerEvent(type);
				initWizardListener(type);
				CountEventTimer.getInstance().start();
			}
		});
	}

	private String getUsedJVM(IJBossToolsEclipseEnvironment environment) {
		return "jvm:" + environment.getJavaVmName() + " / " + environment.getJavaBitVersion();
	}

	public static UsageEventType createFinishWizardType() {
		JBossToolsUsageImplActivator plugin = JBossToolsUsageImplActivator.getDefault();
		String shortVersion = UsageEventType.getVersion(plugin);
		return new UsageEventType("usage", shortVersion, "jbt", "finishWizard", "Wizard class name", "How many times the 'Finish' button pressed during the day");
	}

	private void initWizardListener(UsageEventType type) {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		window.getShell().addListener(SWT.Deactivate, new WizardListener(type));
	}
}