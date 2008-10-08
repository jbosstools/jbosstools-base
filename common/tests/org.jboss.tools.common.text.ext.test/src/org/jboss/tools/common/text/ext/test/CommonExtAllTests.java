package org.jboss.tools.common.text.ext.test;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.jboss.tools.test.util.ProjectImportTestSetup;


public class CommonExtAllTests {
	public static Test suite() {
		TestSuite suite = new TestSuite("Test for default package");
		//$JUnit-BEGIN$

		suite.addTest(new ProjectImportTestSetup(new TestSuite(OpenOnsTest.class),
				"org.jboss.tools.common.text.ext.test",
				new String[]{"projects/HiperlinksTestProject"},
				new String[]{"HiperlinksTestProject"}));
		
		//$JUnit-END$
		return suite;
	}
}
