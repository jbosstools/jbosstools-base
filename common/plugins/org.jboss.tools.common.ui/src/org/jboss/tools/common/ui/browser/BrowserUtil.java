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
package org.jboss.tools.common.ui.browser;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.jboss.tools.common.ui.CommonUIMessages;

/**
 * @author Andre Dietisheim
 */
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
	public static void checkedCreateInternalBrowser(String url, String browserId, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().createBrowser(browserId), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = createErrorStatus(pluginId, CommonUIMessages.BROWSER_COULD_NOT_OPEN_BROWSER, e, url);
			log.log(errorStatus);
		}
	}

	private static IStatus createErrorStatus(String pluginId, String message, Throwable e,
			Object... messageArguments) {
		String formattedMessage = null;
		if (message != null) {
			formattedMessage = MessageFormat.format(message, messageArguments);
		}
		return new Status(Status.ERROR, pluginId, Status.ERROR, formattedMessage, e);
	}

	public static void checkedCreateExternalBrowser(String url, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser(), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = createErrorStatus(pluginId, CommonUIMessages.BROWSER_COULD_NOT_OPEN_BROWSER, e, url);
			log.log(errorStatus);
		}
	}

	public static void openUrl(String url, IWebBrowser browser, String pluginId, ILog log) {
		try {
			browser.openURL(new URL(url));
		} catch (PartInitException e) {
			IStatus errorStatus = createErrorStatus(pluginId, CommonUIMessages.BROWSER_COULD_NOT_OPEN_BROWSER, e, url);
			log.log(errorStatus);
		} catch (MalformedURLException e) {
			IStatus errorStatus = createErrorStatus(pluginId, CommonUIMessages.BROWSER_COULD_NOT_DISPLAY_MALFORMED_URL, e,
					url);
			log.log(errorStatus);
		}
	}
}
