/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui;

import org.eclipse.core.runtime.ILog;
import org.eclipse.ui.browser.IWebBrowser;
import org.jboss.tools.foundation.ui.util.BrowserUtility;

/**
 * @author Andre Dietisheim
 * @deprecated Please use {@link BrowserUtility} instead
 */
@Deprecated
public class BrowserUtil {

	/**
	 * Opens a browser for the given url with the given id. If an error occurs
	 * it will be reported to the given log provider with the given plugin id.
	 * 
	 * @param url
	 *            the url to open a browser for.
	 * @param browserId
	 *            the id for the new browser.
	 * @param pluginId
	 *            the plugin id to log for.
	 * @param log
	 *            the log provider to log against if an error occurred.
	 */
	@Deprecated
	public static void checkedCreateInternalBrowser(String url, String browserId, String pluginId, ILog log) {
		new BrowserUtility().checkedCreateInternalBrowser(url, browserId, pluginId, log);
	}

	@Deprecated
	public static void checkedCreateExternalBrowser(String url, String pluginId, ILog log) {
		new BrowserUtility().checkedCreateExternalBrowser(url, pluginId, log);
	}

	@Deprecated
	public static void openUrl(String url, IWebBrowser browser, String pluginId, ILog log) {
		new BrowserUtility().openUrl(url, browser, pluginId, log);
	}
}
