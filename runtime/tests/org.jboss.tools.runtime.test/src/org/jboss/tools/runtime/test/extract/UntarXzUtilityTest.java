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
import java.io.FileOutputStream;
import java.io.InputStream;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.util.FileUtils;
import org.jboss.tools.runtime.core.extract.ExtractUtility;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class UntarXzUtilityTest {
	
	@Rule
	public TemporaryFolder tmp = new TemporaryFolder();
	
	@Test
	public void testUntar() throws Exception {
		File extractFolder = tmp.getRoot();
		InputStream resourceAsStream = getClass().getResourceAsStream("simpleWithRoot.tar.xz");
		File fileToExtract = new File(extractFolder, "simpleWithRoot.tar.xz");
		FileUtils.copy(resourceAsStream, new FileOutputStream(fileToExtract));
		
		File destination = new File(extractFolder, "destFolder");
		new ExtractUtility(fileToExtract).extract(destination, createOverwrite(), 
				new NullProgressMonitor());
		
		assertThat(new File(destination, "rootFolder/file1.txt")).exists();
		assertThat(new File(destination, "rootFolder/file2.txt")).exists();
		assertThat(new File(destination, "rootFolder/file3.txt")).exists();
	}
	
	private IOverwrite createOverwrite() {
		return new IOverwrite() {
			public int overwrite(File file) {
				return YES;
			}
		};
	}

}
