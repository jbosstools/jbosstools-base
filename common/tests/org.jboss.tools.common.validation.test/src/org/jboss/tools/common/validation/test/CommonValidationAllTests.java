/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.validation.test;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.eclipse.jdt.internal.core.JavaModelManager;
import org.eclipse.wst.validation.ValidationFramework;
import org.jboss.tools.common.base.test.validation.ValidationExceptionTest;
import org.jboss.tools.test.util.ProjectImportTestSetup;

/**
 * @author Alexey Kazakov
 */
public class CommonValidationAllTests {

	public static Test suite() {
		// it could be done here because it is not needed to be enabled back
		JavaModelManager.getIndexManager().disable();
		ValidationFramework.getDefault().suspendAllValidation(true);

		ValidationExceptionTest.initLogger();

		TestSuite suiteAll = new TestSuite("Common Validation Tests");

		TestSuite suite = new TestSuite(ValidationTest.class.getName());
		suite.addTestSuite(ValidationTest.class);

		ProjectImportTestSetup testSetup = new ProjectImportTestSetup(suite,
				"org.jboss.tools.common.validation.test",
				new String[]{"projects/JavaProject"},
				new String[]{"JavaProject"});
		suiteAll.addTest(testSetup);

		return suiteAll;
	}
}