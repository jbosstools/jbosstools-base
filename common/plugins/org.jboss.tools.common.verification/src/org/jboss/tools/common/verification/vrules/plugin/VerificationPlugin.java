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
package org.jboss.tools.common.verification.vrules.plugin;

import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.model.plugin.IModelPlugin;

public class VerificationPlugin extends BaseUIPlugin implements IModelPlugin {
	static VerificationPlugin plugin;
	public static final String PLUGIN_ID = "org.jboss.tools.common.verification"; //$NON-NLS-1$
	public VerificationPlugin() {		
		super();
		plugin = this;
	}
	
	public static boolean isDebugEnabled() {
		return plugin != null && plugin.isDebugging();
	}
	
	public static VerificationPlugin getDefault() {
		return plugin;
	}
	
	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}
}