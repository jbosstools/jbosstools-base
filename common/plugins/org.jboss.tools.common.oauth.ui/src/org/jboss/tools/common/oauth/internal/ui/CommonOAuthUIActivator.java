/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.internal.ui;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.foundation.core.plugin.BaseCorePlugin;

public class CommonOAuthUIActivator extends BaseCorePlugin {

	private static CommonOAuthUIActivator INSTANCE = null;

	public static final String PLUGIN_ID = "org.jboss.tools.common.oauth.ui";

	public CommonOAuthUIActivator() {
		super();
		INSTANCE = this;
	}

	public static CommonOAuthUIActivator getDefault() {
		return INSTANCE;
	}

	public static void logError(String message, Throwable t) {
		getDefault().pluginLogInternal().logError(message, t);
	}

	public void logInfo(String message) {
		IStatus status = new Status(IStatus.INFO, PLUGIN_ID, message);
		getLog().log(status);
	}

}
