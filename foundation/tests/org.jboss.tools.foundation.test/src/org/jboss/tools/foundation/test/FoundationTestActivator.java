/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.test;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class FoundationTestActivator extends Plugin {

	public static final String PLUGIN_ID = "org.jboss.tools.foundation.core.test";
	
	// The shared instance
	private static FoundationTestActivator plugin;
	private static BundleContext context;
	
	/**
	 * The constructor
	 */
	public FoundationTestActivator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		this.context = context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static FoundationTestActivator getDefault() {
		return plugin;
	}
	
	public static BundleContext getContext() {
		return context;
	}
	
}
