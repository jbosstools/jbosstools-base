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
package org.jboss.tools.common.model.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.jboss.common.model.ui.test.preferences.ModelUiPreferencesPageTest;
import org.jboss.tools.common.model.ui.dialog.test.ErrorDialogTest;
import org.jboss.tools.common.model.ui.dialog.test.MessageAndCheckboxDialogTest;
import org.jboss.tools.common.model.ui.jarproperties.JarPropertiesTest;
import org.jboss.tools.common.model.ui.preferences.DecoratorPreferencesPage;
import org.jboss.tools.common.model.ui.reporting.ReportProblemWizardTest;

/**
 * @author eskimo
 *
 */
public class ModelUiAllTests {
	public static final String PLUGIN_ID = "org.jboss.tools.common.model.ui";

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.setName("All tests for " + PLUGIN_ID);
		suite.addTestSuite(ModelUiPreferencesPageTest.class);
		suite.addTestSuite(ObjectDecoratorTest.class);
		suite.addTestSuite(ErrorDialogTest.class);
		suite.addTestSuite(MessageAndCheckboxDialogTest.class);
		//suite.addTestSuite(ReportProblemWizardTest.class); 
		// FIXME does not run in Tycho-based build: 
		//     ERROR in ../common/tests/org.jboss.tools.common.model.ui.test/
		//     src/org/jboss/tools/common/model/ui/reporting/ReportProblemWizardTest.java (at line 17)
		//     [exec]     import junit.extensions.ExceptionTestCase;
		//     [exec]            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
		//     [exec] The import junit.extensions.ExceptionTestCase cannot be resolved
		suite.addTestSuite(JarPropertiesTest.class);
		return suite;
	}

}
