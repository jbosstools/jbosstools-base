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
package org.jboss.tools.common.model.ui.action.global;

import java.net.MalformedURLException;
import java.net.URL;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public abstract class AbstractShowUrlActionDelegate implements IWorkbenchWindowActionDelegate {
	IWorkbenchWindow window;

	public void init(IWorkbenchWindow window) {
		this.window = window;
	}

	public void run(IAction action) {
		try {
			runURL(getUrl());
		} catch (Exception e) {
			ProblemReportingHelper.reportProblem(ModelUIPlugin.PLUGIN_ID, e);
		}
	}

	protected abstract String getUrl();

	public void selectionChanged(IAction action, ISelection selection) {}

	public void dispose() {}

	public static void runURL(String url) {
		if(url == null || url.length() == 0) return;
		if(!url.startsWith("http://")) url = "http://" + url;
		try {
			IWorkbenchBrowserSupport browserSupport = ModelUIPlugin.getDefault().getWorkbench().getBrowserSupport();
			IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR | IWorkbenchBrowserSupport.AS_EXTERNAL, null, null, null);
			browser.openURL(new URL(url));
		} catch (MalformedURLException mue) {
			ServiceDialog d = PreferenceModelUtilities.getPreferenceModel().getService();
			d.showDialog("Error", "Incorrect URL: " + mue.getMessage() + ".", new String[]{"OK"}, null, ServiceDialog.ERROR);
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
	}

}