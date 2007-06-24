/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.test;

import org.jboss.tools.common.util.test.HttpUtilTest;
import org.jboss.tools.common.xml.test.SAXValidatorTest;
import org.jboss.tools.common.xml.test.XMLUtilitiesTest;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class CommonAllTests extends TestCase {
	
	public static Test suite ()
	{
		TestSuite suite = new TestSuite(CommonAllTests.class.getName());
		
		suite.addTestSuite(HttpUtilTest.class);
		suite.addTestSuite(XMLUtilitiesTest.class);
		suite.addTestSuite(SAXValidatorTest.class);
		return suite;
	}
	
}