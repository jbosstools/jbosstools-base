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

/**
 * @deprecated  This interface should not be used any longer. 
 * 				Please use the supported versions in org.jboss.tools.foundation
 */
public interface IPluginLog extends org.jboss.tools.foundation.plugin.log.IPluginLog {
	public void logError(String message, Throwable t);
	public void logError(String message);
	public void logError(Throwable t);

	public void logWarning(String message, Throwable t);	
	public void logWarning(String message);	
	public void logWarning(Throwable t);
	
	public void logInfo(String message, Throwable t);	
	public void logInfo(String message);
}
