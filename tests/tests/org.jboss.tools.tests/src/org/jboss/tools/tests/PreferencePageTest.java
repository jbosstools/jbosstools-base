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
package org.jboss.tools.tests;

import junit.framework.TestCase;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * @author eskimo
 *
 */
public class PreferencePageTest extends TestCase {

	private String prefPageId;
	Class<?extends PreferencePage> instanceOf;
	
	public PreferencePageTest() {
		super();
	}
	
	public PreferencePageTest(String prefPageId, Class<?extends PreferencePage> instanceOf) {
		this.prefPageId = prefPageId;
		this.instanceOf = instanceOf;
	}
	/**
	 * This method performs simple test for provided preference page. It 
	 * opens preference dialog with page loaded by a provided prefPageId
	 * and verifies that it is an instance of provided class
	 */
	public void doDefaultTest(String id, Class instanceOf) {
		PreferenceDialog prefDialog = 
			WorkbenchUtils.createPreferenceDialog(
					id);

		try {
			prefDialog.setBlockOnOpen(false);
			prefDialog.open();
			
			Object selectedPage = prefDialog.getSelectedPage();
			assertTrue("Selected page is not an instance of PreferencePage", instanceOf.isInstance(selectedPage)); //$NON-NLS-1$
		} finally {
			prefDialog.close();
		}
	}
	
	public void doDefaultTest() {
		doDefaultTest(prefPageId,instanceOf);
	}
}
