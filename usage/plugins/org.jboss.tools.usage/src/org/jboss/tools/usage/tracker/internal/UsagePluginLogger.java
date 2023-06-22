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

/**
 * @author Andre Dietisheim
 */
public class UsagePluginLogger {

	private Plugin plugin;

	public UsagePluginLogger(Plugin plugin) {
		this.plugin = plugin;
	}

	public void warn(String message, Throwable t) {
		log(IStatus.WARNING, message, t, false);
	}

	public void error(String message) {
		error(message, true);
	}

	public void error(String message, boolean debug) {
		log(IStatus.ERROR, message, null, debug);
	}

	public void error(Throwable t) {
		error(t, false);
	}

	public void error(Throwable t, boolean debug) {
		if (debug && !isTracingEnabled()) {
			return;
		}

		log(IStatus.ERROR, null, t, debug);
	}

	public void debug(String message) {
		log(IStatus.INFO, message, null, true);
	}

	private void log(int severity, String message, Throwable t, boolean debug) {
		if (debug && !isTracingEnabled()) {
			return;
		}

		if (message == null) {
			message = t != null && t.getMessage() != null ? t.getMessage() : "";
		}
		
		if (plugin != null) {
			IStatus status = new Status(severity, plugin.getBundle().getSymbolicName(), message, t);
			plugin.getLog().log(status);
		}
	}

		
	protected boolean isTracingEnabled() {
		Plugin plugin = getPlugin();
		return plugin != null && plugin.isDebugging();
	}

	protected Plugin getPlugin() {
		return plugin;
	}

}
