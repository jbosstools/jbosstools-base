/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 *     Zend Technologies Ltd. - JBIDE-18678
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.util.UsagePluginLogger;
import org.osgi.framework.BundleContext;

/**
 * @author Andre Dietisheim
 * @author Kaloyan Raev
 */
public class JBossToolsUsageActivator extends Plugin {

	public static final String PLUGIN_ID = "org.jboss.tools.usage"; //$NON-NLS-1$

	private static JBossToolsUsageActivator plugin;

	UsagePluginLogger logger;

	public JBossToolsUsageActivator() {
		plugin = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	public static JBossToolsUsageActivator getDefault() {
		return plugin;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		logger = new UsagePluginLogger(JBossToolsUsageActivator.getDefault());
	}

	public UsagePluginLogger getLogger() {
		return logger;
	}

}
