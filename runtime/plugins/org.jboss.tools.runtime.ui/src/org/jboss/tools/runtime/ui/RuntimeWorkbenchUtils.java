package org.jboss.tools.runtime.ui;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.navigator.CommonNavigator;
import org.jboss.tools.runtime.ui.preferences.RuntimePreferencePage;

public class RuntimeWorkbenchUtils {
	public static void refreshServersView() {
		// https://jira.jboss.org/jira/browse/JBDS-1091
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				IViewPart view = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.wst.server.ui.ServersView");
				if (view instanceof CommonNavigator) {
					CommonNavigator navigator = (CommonNavigator) view;
					navigator.getCommonViewer().refresh();
				}
			}
		});
	}
	
	public static void refreshPreferencePage(final Shell shell) {
		Display.getCurrent().asyncExec(new Runnable() {
			public void run() {
				refreshPreferencePageUIThread(shell);
			}
		});
	}
	
	public static void refreshPreferencePageUIThread(Shell shell) {
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
						RuntimePreferencePage.ID, null, null);
		preferenceDialog.open();
	}

}
