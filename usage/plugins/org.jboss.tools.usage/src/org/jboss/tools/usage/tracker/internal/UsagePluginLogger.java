/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.tracker.internal;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.common.log.PluginLogger;
import org.jboss.tools.usage.util.LoggingUtils;

/**
 * @author Andre Dietisheim
 *
 */
public class UsagePluginLogger extends PluginLogger {

	public UsagePluginLogger(Plugin plugin) {
		super(plugin);
	}

	protected boolean isTracingEnabled() {
		return LoggingUtils.isPluginTracingEnabled(getPlugin());
	}
}
