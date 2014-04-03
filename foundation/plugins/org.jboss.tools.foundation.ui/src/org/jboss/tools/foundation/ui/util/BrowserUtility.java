/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.util;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;

public class BrowserUtility {

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
	public void checkedCreateInternalBrowser(String url, String browserId, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, browserId, null, null), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}
	
	public void checkedCreateExternalBrowser(String url, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser(), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}

	public void openUrl(String url, IWebBrowser browser, String pluginId, ILog log) {
		try {
			browser.openURL(new URL(url));
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		} catch (MalformedURLException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not display malformed url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}
}
