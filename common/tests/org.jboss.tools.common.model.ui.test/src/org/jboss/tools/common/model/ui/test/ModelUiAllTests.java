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

import org.jboss.tools.common.core.jdt.test.FavoritesClassControllerTest;

import junit.framework.Test;
import junit.framework.TestSuite;

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
		suite.addTestSuite(FavoritesClassControllerTest.class);
		return suite;
	}

}
