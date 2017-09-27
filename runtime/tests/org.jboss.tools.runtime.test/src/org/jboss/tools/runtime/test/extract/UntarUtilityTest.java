/*******************************************************************************
 * Copyright (c) 2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime.test.extract;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ui.internal.wizards.datatransfer.TarEntry;
import org.eclipse.ui.internal.wizards.datatransfer.TarOutputStream;
import org.jboss.tools.runtime.core.extract.ExtractUtility;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class UntarUtilityTest {
	
	private static final String DUMMY_FILE_CONTENT = "this is a file";
	@Rule
	public TemporaryFolder tmpFolder = new TemporaryFolder();
	

	@Test
	public void runExtractTest() throws Exception {
		File tmpFolderRoot = tmpFolder.getRoot();
		File outTar = new File(tmpFolderRoot, "out.tar");
		File f = createTarFile(outTar, new String[]{"out1.txt", "out2.txt", "same.name", "out3.txt", }, new String[] {"same.name"});
		File dest = new File(tmpFolderRoot, "out");
		dest.mkdirs();
		new ExtractUtility(f).extract(dest, createOverwrite(), new NullProgressMonitor());
		assertTrue(new File(dest, "out1.txt").exists());
		assertTrue(new File(dest, "out2.txt").exists());
		assertTrue(new File(dest, "out3.txt").exists());
	}
	
	private File createTarFile(File out, String[] fileEntries, String[] folderEntries) throws Exception {
		out.getParentFile().mkdirs();
		try (FileOutputStream fos = new FileOutputStream(out); TarOutputStream tos = new TarOutputStream(fos); ) {
			for (String folderEntry : folderEntries) {
				TarEntry tarEntry = new TarEntry(folderEntry);
				tarEntry.setFileType(TarEntry.DIRECTORY);
				tos.putNextEntry(tarEntry);
				tos.closeEntry();
			}
			for(String fileEntry : fileEntries) {
				addToTarFile(toIS(DUMMY_FILE_CONTENT), fileEntry, tos);
			}
		}
		return out;
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
	
	public static void addToTarFile(InputStream source, String entryName, TarOutputStream tos) throws FileNotFoundException, IOException {
		System.out.println(entryName);
		TarEntry tarEntry = new TarEntry(entryName);
		tarEntry.setSize(DUMMY_FILE_CONTENT.length());
		tos.putNextEntry(tarEntry);
		
		byte[] bytes = new byte[2048];
		int length;
		while ((length = source.read(bytes)) > 0) {
			tos.write(bytes, 0, length);
		}

		tos.closeEntry();
		source.close();
	}
	
}
