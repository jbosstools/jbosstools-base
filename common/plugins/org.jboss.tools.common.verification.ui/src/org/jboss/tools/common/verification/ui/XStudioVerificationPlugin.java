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
package org.jboss.tools.common.verification.ui;

import org.eclipse.core.runtime.QualifiedName;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.model.plugin.IModelPlugin;

public class XStudioVerificationPlugin extends BaseUIPlugin implements IModelPlugin {

	// The shared instance
	private static XStudioVerificationPlugin plugin;
	
	public final static String PLUGIN_ID = "org.jboss.tools.common.verification.ui"; //$NON-NLS-1$
	
	public static final QualifiedName RESOURCE_EXCLUDED = new QualifiedName(PLUGIN_ID, "resourceExcluded"); //$NON-NLS-1$

	public XStudioVerificationPlugin() {
	    super();
	    plugin = this;
	}
	
	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static XStudioVerificationPlugin getDefault() {
		return plugin;
	}
	
	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.log.BaseUIPlugin#getId()
	 */
	@Override
	public String getId() {
		return PLUGIN_ID;
	}
}