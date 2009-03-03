/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.verification.ui.vrules.preferences.test;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.jboss.tools.common.verification.ui.vrules.preferences.VerificationPreferencePage;
import org.jboss.tools.test.util.SwtUtils;
import org.jboss.tools.test.util.WorkbenchUtils;
import org.jboss.tools.tests.PreferencePageTest;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class VerificationPreferencePageTest extends PreferencePageTest {

	public static final String ID = "org.jboss.tools.common.verification.ui";
	
	public void testShowVerificationPreferencePage() {
		
		doDefaultTest(ID, VerificationPreferencePage.class);
		
		PreferenceDialog dialog = null;
		try {
			dialog = WorkbenchUtils.createPreferenceDialog(ID);
			dialog.open();
			VerificationPreferencePage page = (VerificationPreferencePage)dialog.getSelectedPage();
			TabFolder tabs = (TabFolder)SwtUtils.findControlByClass((Composite)page.getControl(), TabFolder.class);
			tabs.setSelection(0);
			tabs.setSelection(1);
		} finally {
			dialog.close();
		}
	}

}
