/*******************************************************************************
  * Copyright (c) 2016 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.runtime.test.extract;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.util.FileUtils;
import org.jboss.tools.runtime.core.extract.ExtractUtility;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.jboss.tools.runtime.test.RuntimeTestActivator;
import org.junit.After;

import junit.framework.TestCase;

public class UnzipUtilityTest extends TestCase {
	
	public void testExtractNPE() throws Exception {
		runExtractTest("out");
	}
	
	@After
	public void cleanup() {
		FileUtils.clear(RuntimeTestActivator.getDefault().getStateLocation().toFile());
	}
	
	private void runExtractTest(String name) throws Exception {
		IPath outZip = RuntimeTestActivator.getDefault().getStateLocation().append(name + ".zip");
		File f = createZipFile1(outZip, new String[]{"out1.txt", "out2.txt", "out3.txt"});
		IPath dest = RuntimeTestActivator.getDefault().getStateLocation().append(name);
		dest.toFile().mkdirs();
		new ExtractUtility(f).extract(dest.toFile(), createOverwrite(), new NullProgressMonitor());
		assertTrue(dest.append("out1.txt").toFile().exists());
		assertTrue(dest.append("out2.txt").toFile().exists());
		assertTrue(dest.append("out3.txt").toFile().exists());
	}
	
	private File createZipFile1(IPath out, String[] fileEntries) throws Exception {
		String c = "this is a file";
		out.toFile().getParentFile().mkdirs();
		File f = out.toFile();
		try (FileOutputStream fos = new FileOutputStream(f); ZipOutputStream zos = new ZipOutputStream(fos); ) {
			for( int i = 0; i < fileEntries.length; i++ ) {
				addToZipFile(toIS(c), fileEntries[i], zos);
			}
		}
		return f;
	}

	private IOverwrite createOverwrite() {
		return new IOverwrite() {
			public int overwrite(File file) {
				return YES;
			}
		};
	}
	
	private InputStream toIS(String s) {
		return new ByteArrayInputStream(s.getBytes());
	}

	public static void addToZipFile(InputStream source, String entryName, ZipOutputStream zos) throws FileNotFoundException, IOException {
		ZipEntry zipEntry = new ZipEntry(entryName);
		zos.putNextEntry(zipEntry);

		byte[] bytes = new byte[1024];
		int length;
		while ((length = source.read(bytes)) >= 0) {
			zos.write(bytes, 0, length);
		}

		zos.closeEntry();
		source.close();
	}
}
