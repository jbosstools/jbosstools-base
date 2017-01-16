/*******************************************************************************
 * Copyright (c) 2016-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;

public class FileUtils {

	public static void deleteDirectory(File dir) throws IOException {
		if (!dir.exists()) {
			return;
		}
		Files.walkFileTree(Paths.get(dir.getAbsolutePath()), new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
				Files.delete(file);
				return FileVisitResult.CONTINUE;
			}

			@Override
			public FileVisitResult postVisitDirectory(Path directory, IOException exc) throws IOException {
				Files.delete(directory);
				return FileVisitResult.CONTINUE;
			}
		});
	}

	public static void copyFileIntoProjectFolder(String projectName, File file) {
		FileChannel inChannel = null;
		FileChannel outChannel = null;

		List<File> libraryFiles = new ArrayList<File>();
		FileInputStream istream = null;
		FileOutputStream ostream = null;
		try {
			File out = new File(Platform.getLocation() + File.separator + projectName + File.separator + File.separator
					+ file.getName());

			istream = new FileInputStream(file);
			ostream = new FileOutputStream(out);

			inChannel = istream.getChannel();
			outChannel = ostream.getChannel();

			inChannel.transferTo(0, inChannel.size(), outChannel);
			libraryFiles.add(file);
		} catch (IOException ioException) {

		} finally {
			try {
				if (istream != null) {
					istream.close();
				}
				if (ostream != null) {
					ostream.close();
				}
			} catch (IOException ex) {

			}
		}
	}
}
