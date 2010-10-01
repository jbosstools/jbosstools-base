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
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.LoggingUtils;
import org.jboss.tools.usage.util.StatusUtils;

/**
 * @author Andre Dieitsheim
 */
public class UsageReportDispatcher implements IStartup {

	public void earlyStartup() {
		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				try {
					new UsageReport().report();
				} catch (Exception e) {
					IStatus status = StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID, "could not start usage reporting", e);
					LoggingUtils.log(status, JBossToolsUsageActivator.getDefault());
				}
			}
		});
	}
}
