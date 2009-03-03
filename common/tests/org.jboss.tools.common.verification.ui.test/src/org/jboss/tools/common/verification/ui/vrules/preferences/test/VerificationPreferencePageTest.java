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
import org.jboss.tools.common.verification.ui.vrules.preferences.VerificationPreferencePage;
import org.jboss.tools.test.util.WorkbenchUtils;
import org.jboss.tools.tests.PreferencePageTest;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class VerificationPreferencePageTest extends PreferencePageTest {

	
	public void testShowVerificationPreferencePage() {
		doDefaultTest(
			"org.jboss.tools.common.verification.ui",
			VerificationPreferencePage.class);
	}
}
