/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageTestActivator extends Plugin {

	public static final String PLUGIN_ID = "org.jboss.tools.usage.tests"; //$NON-NLS-1$

	private static JBossToolsUsageTestActivator plugin;
	
	public JBossToolsUsageTestActivator() {
		plugin = this;
	}

	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	public static JBossToolsUsageTestActivator getDefault() {
		return plugin;
	}
}