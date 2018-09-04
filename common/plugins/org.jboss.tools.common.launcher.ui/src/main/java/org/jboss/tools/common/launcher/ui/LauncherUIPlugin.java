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
package org.jboss.tools.common.launcher.ui;

import org.jboss.tools.common.log.BaseUIPlugin;

public class LauncherUIPlugin extends BaseUIPlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.common.launcher.ui"; //$NON-NLS-1$
	
	private static LauncherUIPlugin INSTANCE;
	
	public LauncherUIPlugin() {
		INSTANCE = this;
	}
	
	public static LauncherUIPlugin getDefault() {
		return INSTANCE;
	}
}
