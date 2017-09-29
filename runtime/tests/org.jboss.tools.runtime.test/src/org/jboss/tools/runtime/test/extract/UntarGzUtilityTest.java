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

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.util.FileUtils;
import org.jboss.tools.runtime.core.extract.ExtractUtility;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class UntarGzUtilityTest {
	
	@Rule
	public TemporaryFolder tmp = new TemporaryFolder();
	private ExtractUtility extractUtility;
	
	@Test
	public void testSimple() throws Exception {
		File destination = extract("simpleWithRoot.tar.gz");
		checkExtraction(destination, "rootFolder");
	}

	@Test
	public void testWithPaxHeaders() throws Exception {
		File destination = extract("deepFoldersWithRootAndPax.tar.gz");
		checkExtraction(destination, "rootFolder/with/deep/nested/folder");
	}
	
	@Test
	public void testWithDeepFolders() throws Exception {
		File destination = extract("deepFoldersWithRoot.tar.gz");
		checkExtraction(destination, "rootFolder/with/deep/nested/folder");
	}
	
	protected void checkExtraction(File destination, String pathToFiles) throws CoreException {
		assertThat(new File(destination, pathToFiles+"/file1.txt")).exists();
		assertThat(new File(destination, pathToFiles+"/file2.txt")).exists();
		assertThat(new File(destination, pathToFiles+"/file3.txt")).exists();
		assertThat(extractUtility.getExtractedRootFolder(new NullProgressMonitor())).isEqualTo("rootFolder");
	}

	protected File extract(String fileNameToExtract) throws IOException, FileNotFoundException {
		File extractFolder = tmp.getRoot();
		File fileToExtract = createFileToExtract(extractFolder, fileNameToExtract);
		
		File destination = new File(extractFolder, "destFolder");
		extractUtility = new ExtractUtility(fileToExtract);
		extractUtility.extract(destination, createOverwrite(), new NullProgressMonitor());
		return destination;
	}

	protected File createFileToExtract(File extractFolder, String fileNameToExtract)
			throws IOException, FileNotFoundException {
		InputStream resourceAsStream = getClass().getResourceAsStream(fileNameToExtract);
		File fileToExtract = new File(extractFolder, fileNameToExtract);
		FileUtils.copy(resourceAsStream, new FileOutputStream(fileToExtract));
		return fileToExtract;
	}
	
	private IOverwrite createOverwrite() {
		return new IOverwrite() {
			public int overwrite(File file) {
				return YES;
			}
		};
	}
}
