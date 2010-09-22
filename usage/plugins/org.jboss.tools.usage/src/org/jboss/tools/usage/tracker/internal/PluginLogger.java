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
package org.jboss.tools.usage.tracker.internal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.usage.tracker.ILoggingAdapter;
import org.jboss.tools.usage.util.LoggingUtils;

/**
 * @author Andre Dietisheim
 */
public class PluginLogger implements ILoggingAdapter {

	private final boolean tracingEnabled;

	private Plugin plugin;

	public PluginLogger(Plugin plugin) {
		this.tracingEnabled = LoggingUtils.isPluginTracingEnabled(plugin);
		this.plugin = plugin;
	}

	public void logError(String message) {
		log(IStatus.ERROR, message);
	}

	public void logMessage(String message) {
		log(IStatus.INFO, message);
	}

	private void log(int severity, String message) {
		if (!tracingEnabled) {
			return;
		}

		if (plugin != null) {
			IStatus status = new Status(severity, plugin.getBundle().getSymbolicName(), message);
			plugin.getLog().log(status);
		}
	}
}
