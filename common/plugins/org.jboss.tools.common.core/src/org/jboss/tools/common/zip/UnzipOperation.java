/*******************************************************************************
  * Copyright (c) 2009 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.zip;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class UnzipOperation {
	
	File zipFile;
	
	public static final int BUFFER_SIZE = 1024*4;
	
	public UnzipOperation(String zipFile) {
		this(new File(zipFile));
	}
	
	public UnzipOperation(File zipFile) {
		this.zipFile = zipFile;
	}
	
	public void execute(File destination) throws IOException {
		ZipArchive archive = new ZipArchive(zipFile);
		archive.acceptVisitor(new UnzipEntryVisitor(destination));
	}
	
	public void execute(String destination) throws IOException {
		execute(new File(destination));
	}

	public void execute(File destination,String filter) throws IOException {
		ZipArchive archive = new ZipArchive(zipFile);
		archive.acceptVisitor(new FilteredZipEntryVisitor(filter,new UnzipEntryVisitor(destination)));
	}
	
	public void execute(String destination,String filter) throws IOException {
		execute(new File(destination,filter));
	}
	
	public static class FilteredZipEntryVisitor implements IZipEntryVisitor{

		private String filter;
		private IZipEntryVisitor visitor;

		public FilteredZipEntryVisitor(String filter, IZipEntryVisitor visitor) {
			this.filter = filter;
			this.visitor = visitor;
		}
		
		public void visiteDirectoryEntry(ZipFile zipFile, ZipEntry dir) throws IOException {
			if(dir.getName().matches(filter)) {
				visitor.visiteDirectoryEntry(zipFile, dir);
			}
		}

		public void visiteFileEntry(ZipFile zipFile, ZipEntry file) throws IOException {
			if(file.getName().matches(filter)) {
				visitor.visiteFileEntry(zipFile, file);
			}
		}
	}

	public static class UnzipEntryVisitor implements IZipEntryVisitor {

		private File destination;

		public UnzipEntryVisitor(File destination) {
			this.destination = destination;
		}
	
		public void visiteDirectoryEntry(ZipFile zipFIle, ZipEntry dir) throws IOException {
			File entryDir = new File(destination,dir.getName());
			entryDir.mkdirs();
		}

		public void visiteFileEntry(ZipFile zipFile, ZipEntry file) throws IOException {
			File outputFile = new File(destination,file.getName());
			outputFile.getParentFile().mkdirs();
			outputFile.createNewFile();
			BufferedOutputStream out = null;
			BufferedInputStream in =null; 
			try {
				out = new BufferedOutputStream(new FileOutputStream(outputFile),BUFFER_SIZE);
				in= new BufferedInputStream(zipFile.getInputStream(file));
				byte[] buff = new byte[BUFFER_SIZE];
				int n = -1;
				while((n = in.read(buff,0,buff.length))>-1) {
					out.write(buff, 0, n);
				}
				out.flush();
			} finally {
				if (in != null) {
					in.close();
				}
				if (out != null) {
					out.close();
				}
			}
		}
	}
}
