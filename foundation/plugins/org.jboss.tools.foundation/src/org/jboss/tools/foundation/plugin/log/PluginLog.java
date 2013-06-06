/*******************************************************************************
 * Copyright (c) 2013 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * plugin program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies plugin distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.plugin.log;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;

/**
 * An implementation of a plugin log.
 * 
 * This class is not intended to be subclassed by clients.
 * It is expected clients will make use of BaseCorePlugin and receive
 * functionality that way, though you may instantiate it if you cannot
 * subclass BaseCorePlugin. 
 * 
 * This is the only approved implementation of IPluginLog
 * 
 * @noextend This class is not intended to be subclassed by clients.
 */
public class PluginLog implements IPluginLog {
	private Plugin plugin;
	private String pluginId;
	public PluginLog(Plugin plugin) {
		this.plugin = plugin;
		this.pluginId = plugin.getBundle().getSymbolicName();
	}
	
	public void logError(String message, Throwable t) {
		logStatus(StatusFactory.errorStatus(pluginId, message, t));
	}

	public void logError(String message) {
		logStatus(StatusFactory.errorStatus(pluginId, message));
	}
	
	public void logError(Throwable t) {
		logStatus(StatusFactory.errorStatus(pluginId, t.getMessage(), t));
	}

	public void logWarning(String message, Throwable t) {
		logStatus(StatusFactory.warningStatus(pluginId, t.getMessage(), t));
	}
	
	public void logWarning(String message) {
		logStatus(StatusFactory.warningStatus(pluginId, message));
	}
	
	public void logWarning(Throwable t) {
		logStatus(StatusFactory.warningStatus(pluginId, t.getMessage(), t));
	}

	public void logInfo(String message, Throwable t) {
		logStatus(StatusFactory.infoStatus(pluginId, t.getMessage(), t));
	}
	
	public void logInfo(String message) {
		logStatus(StatusFactory.infoStatus(pluginId, message));
	}
	
	public void logMessage(int code, String message, Throwable t) {
		if(t==null) {
			logInfo(message);
		} else {
			IStatus s = StatusFactory.throwableToStatus(plugin.getBundle().getSymbolicName(), t, code);
			logStatus(s);
		}
	}
	
	public void logStatus(IStatus s) {
		plugin.getLog().log(s);
	}
}
