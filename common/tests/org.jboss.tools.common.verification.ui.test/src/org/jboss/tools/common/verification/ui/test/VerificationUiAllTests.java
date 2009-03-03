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

package org.jboss.tools.common.verification.ui.test;

import org.jboss.tools.common.verification.ui.vrules.preferences.test.VerificationPreferencePageTest;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author eskimo
 *
 */
public class VerificationUiAllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.setName("Verification UI Test");
		suite.addTestSuite(VerificationPreferencePageTest.class);
		return suite;
	}
	
}
