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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;
import org.jboss.tools.usage.installation.CompatibilityChecker;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;

/**
 * @author Andre Dieitsheim
 */
public class UsageReportDispatcher implements IStartup {

	public void earlyStartup() {
		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				try {
					new UsageReport().report();
					new CompatibilityChecker().check();
				} catch (Exception e) {
					IStatus status = new Status(IStatus.ERROR, JBossToolsUsageActivator.PLUGIN_ID, 0, "could not start usage reporting", e);
					JBossToolsUsageActivator.getDefault().getLog().log(status);
//					new UsagePluginLogger(JBossToolsUsageActivator.getDefault()).error("could not start usage reporting");
				}
			}
		});
	}
}