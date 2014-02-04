/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.usage;

public interface IUsageTracker {
	// categories
	String CATEGORY_CENTRAL = "org.jboss.tools.central"; //$NON-NLS-1$
	
	// actions
	// value of preference "Show JBoss Central On Startup"
	String ACTION_SHOW_ON_STARTUP = "showOnStartup"; //$NON-NLS-1$

	// software id installed from tab "Software/Update" of JBoss Central
	String ACTION_INSTALLED_SOFTWARE = "installedSoftware"; //$NON-NLS-1$
	
	// project example name installed from tab "Getting Started" of JBoss Central
	String ACTION_INSTALLED_EXAMPLE = "installedExample"; //$NON-NLS-1$
	
	// project example name failed to install from tab "Getting Started" of JBoss Central
	String ACTION_FAILED_INSTALL_EXAMPLE = "failedInstallExample"; //$NON-NLS-1$
	
	/*
	 * sends one report per session 
	 */
	public void sendDailyEvent(String eventCategory, String eventAction, String eventLabel);
	
	/*
	 * sends report 
	 */
	public void sendLiveEvent(String eventCategory, String eventAction, String eventLabel);
}
