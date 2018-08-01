/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.core;

import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ENDPOINT_PREFERENCE_DEFAULT;
import static org.jboss.tools.common.launcher.core.LauncherCoreConstants.LAUNCHER_ENDPOINT_PREFERENCE_NAME;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.jboss.tools.common.log.BaseUIPlugin;

public class LauncherCorePlugin extends BaseUIPlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.common.launcher.core"; //$NON-NLS-1$
	
	private static LauncherCorePlugin INSTANCE;
	
	public LauncherCorePlugin() {
		INSTANCE = this;
	}
	
	public static LauncherCorePlugin getDefault() {
		return INSTANCE;
	}
	
	public String getDefaultEndpointURL() {
		return InstanceScope.INSTANCE.getNode(PLUGIN_ID).get(LAUNCHER_ENDPOINT_PREFERENCE_NAME, LAUNCHER_ENDPOINT_PREFERENCE_DEFAULT);
	}
}
