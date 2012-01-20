/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
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
