/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.log;

import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.foundation.plugin.BaseCorePlugin;

/**
 * Provides an easy way to log status of events.
 * 
 * NOTE: It is useful to make the static method getPluginLog()
 * which provides the interface IPluginLog for using it in your code
 * in future
 *  
 * @author Sergey Vasilyev
 * @deprecated it is better to use org.jboss.tools.foundation.BaseCorePlugin
 */

public class BasePlugin extends BaseCorePlugin implements IPluginLog {
	public void logError(String message, Throwable t) {
		LogHelper.logError(this, message, t);
	}

	public void logError(String message) {
		LogHelper.logError(this, message);
	}
	
	public void logError(Throwable t) {
		LogHelper.logError(this, t);
	}

	public void logWarning(String message, Throwable t) {
		LogHelper.logWarning(this, message, t);
	}
	
	public void logWarning(String message) {
		LogHelper.logWarning(this, message);
	}
	
	public void logWarning(Throwable t) {
		LogHelper.logWarning(this, t);
	}

	public void logInfo(String message, Throwable t) {
		LogHelper.logInfo(this, message, t);
	}
	
	public void logInfo(String message) {
		LogHelper.logInfo(this, message);
	}

	@Override
	public void logMessage(int code, String message, Throwable t) {
		getPluginLog().logMessage(code, message, t);
	}

	public void logStatus(IStatus status) {
		getPluginLog().logStatus(status);
	}
}