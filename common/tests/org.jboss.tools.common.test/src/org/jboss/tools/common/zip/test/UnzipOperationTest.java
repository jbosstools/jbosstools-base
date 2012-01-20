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

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.zip.UnzipOperation;

public class UnzipOperationTest extends AbstractZipTestCase {

	private static final String ORG_PACKAGE_FILTER = "org.*";

	public void testUnzipOperationExecuteFile() throws IOException {
		UnzipOperation unzip = new UnzipOperation(getZip().getAbsolutePath());
		File destination = new File(getTemp(),Long.toString(System.currentTimeMillis()));
		destination.mkdirs();
		unzip.execute(destination);
	}

	public void testUnzipOperationExecuteFileString() throws IOException {
		UnzipOperation unzip = new UnzipOperation(getZip().getAbsolutePath());
		File destination = new File(getTemp(),Long.toString(System.currentTimeMillis()));
		destination.mkdirs();
		unzip.execute(destination,ORG_PACKAGE_FILTER);
	}

	public void testUnzipOperationExecuteString() throws IOException {
		UnzipOperation unzip = new UnzipOperation(getZip());
		File destination = new File(getTemp(),Long.toString(System.currentTimeMillis()));
		destination.mkdirs();
		unzip.execute(destination.getAbsolutePath());
	}

	public void testUnzipOperationExecuteStringString() throws IOException {
		UnzipOperation unzip = new UnzipOperation(getZip());
		File destination = new File(getTemp(),Long.toString(System.currentTimeMillis()));
		destination.mkdirs();
		unzip.execute(destination.getAbsolutePath(),ORG_PACKAGE_FILTER);
	}

}
