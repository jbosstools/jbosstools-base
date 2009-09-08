package org.jboss.tools.common.zip.test;

import org.jboss.tools.common.zip.ZipArchive;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class ZipAllTests extends TestCase {
	public static Test suite() {
		TestSuite allTests = new TestSuite("org.jboss.tools.common.zip test suite");
		allTests.addTestSuite(UnzipOperationTest.class);
		allTests.addTestSuite(ZipArchiveTest.class);
		return allTests;
	}
}
