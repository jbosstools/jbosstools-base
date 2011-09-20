package org.jboss.tools.test.util;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import junit.framework.TestCase;

public class PreferencePageAbstractTest extends TestCase {

	public boolean createPreferencePage(String id, Class expectedInstance) {
		PreferenceDialog prefDialog = createPreferenceDialog(id);
		
		try {
			prefDialog.setBlockOnOpen(false);
			prefDialog.open();
			
			Object selectedPage = prefDialog.getSelectedPage();
			return expectedInstance.isInstance(selectedPage); //$NON-NLS-1$
		} finally {
			prefDialog.close();
		}
		
	}
	
	public void pressOkOnPreferencePage(String ID) {
		PreferenceDialog prefDialog = WorkbenchUtils.createPreferenceDialog(ID);
	
		try {
			prefDialog.setBlockOnOpen(false);
			prefDialog.open();
			
			PreferencePage selectedPage = (PreferencePage)prefDialog.getSelectedPage();
			selectedPage.performOk();
		} finally {
			prefDialog.close();
		}
	}

	public static PreferenceDialog createPreferenceDialog(String pageId) {
		PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(
				 PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), pageId, new String[] {pageId}, null);
		dialog.setBlockOnOpen(false);
		return dialog;
	}
	
}
