/******************************************************************************* 
 * Copyright (c) 2007-2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
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
