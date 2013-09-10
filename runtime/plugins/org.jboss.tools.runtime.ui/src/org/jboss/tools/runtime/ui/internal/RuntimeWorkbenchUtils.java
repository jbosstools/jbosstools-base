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
package org.jboss.tools.runtime.ui.internal;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.navigator.CommonNavigator;
import org.jboss.tools.runtime.ui.internal.preferences.RuntimePreferencePage;

public class RuntimeWorkbenchUtils {
	public static void refreshServersView() {
		// https://jira.jboss.org/jira/browse/JBDS-1091
		if (!PlatformUI.isWorkbenchRunning()) {
			return;
		}
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				if (PlatformUI.isWorkbenchRunning()) {
					IViewPart view = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.wst.server.ui.ServersView"); //$NONE-NLS-1$ //$NON-NLS-1$
					if (view instanceof CommonNavigator) {
						CommonNavigator navigator = (CommonNavigator) view;
						navigator.getCommonViewer().refresh();
					}
				}
			}
		});
	}
	
	public static void refreshPreferencePage(final Shell shell) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				refreshPreferencePageUIThread(shell);
			}
		});
	}
	
	public static void refreshPreferencePageUIThread(Shell shell) {
		refreshPreferencePageUIThread(shell, RuntimePreferencePage.ID);
	}
	
	public static void refreshPreferencePageUIThread(Shell shell, String pageId) {

		Shell mainShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if (shell != null && !shell.isDisposed()) {
			shell.close();
		}
		shell = Display.getCurrent().getActiveShell();
		if (shell != mainShell && shell != null) {
			shell.close();
		}
		PreferenceDialog preferenceDialog = PreferencesUtil
				.createPreferenceDialogOn(PlatformUI.getWorkbench()
						.getActiveWorkbenchWindow().getShell(),
						pageId, null, null);
		preferenceDialog.open();
	}

}
