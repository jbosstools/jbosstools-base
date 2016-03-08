/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.extract.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.extract.IOverwrite;

public class UnzipUtility implements IExtractUtility {
	private static String EXTRACTING = "Extracting ..."; //$NON-NLS-1$
	private static final String SEPARATOR = "/"; //$NON-NLS-1$
	
	private File file;
	private String discoveredRoot = null;
	private boolean rootEntryImpossible = false;
	
	public UnzipUtility(File file) {
		this.file = file;
	}
	
	
	public IStatus extract(File destination, IOverwrite overwriteQuery, IProgressMonitor monitor) {
		if( file == null || !file.exists()) {
			return new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, 
					"Error opening zip file: " + file.getAbsolutePath() + "; File does not exist");
		}

		
		String possibleRoot = null;
		
		
		ZipFile zipFile = null;
		int overwrite = IOverwrite.NO;
		destination.mkdirs();
		try {
			zipFile = new ZipFile(file);
			Enumeration<? extends ZipEntry> entries = zipFile.entries();
			monitor.beginTask(EXTRACTING, zipFile.size());
			while (entries.hasMoreElements()) {
				monitor.worked(1);
				if (monitor.isCanceled() || overwrite == IOverwrite.CANCEL) {
					return Status.CANCEL_STATUS;
				}
				ZipEntry entry = (ZipEntry) entries.nextElement();
				String entryName = entry.getName();
				File entryFile = new File(destination, entryName);
				monitor.subTask(entry.getName());
				if (overwrite != IOverwrite.ALL && overwrite != IOverwrite.NO_ALL && entryFile.exists()) {
					overwrite = overwriteQuery.overwrite(entryFile);
					switch (overwrite) {
					case IOverwrite.CANCEL:
						return Status.CANCEL_STATUS;
					default:
						break;
					}
				}
				if (!entryFile.exists() || overwrite == IOverwrite.YES || overwrite == IOverwrite.ALL) {
					createEntry(monitor, zipFile, entry, entryFile);
				}
				
				// Lets check for a possible root, to avoid scanning the archive again later
				if( !rootEntryImpossible && discoveredRoot == null) {
					// Check for a root
					int separatorIdx = -1;
					if (entryName == null || entryName.isEmpty() || entryName.startsWith(SEPARATOR) || (separatorIdx = entryName.indexOf(SEPARATOR)) == -1) {
						rootEntryImpossible = true;
						possibleRoot = null;
					}
					String directory = (separatorIdx > -1) ? entryName.substring(0, separatorIdx) : entryName;
					if (possibleRoot == null) {
						possibleRoot = directory;
					} else if (!directory.equals(possibleRoot)) {
						rootEntryImpossible = true;
						possibleRoot = null;
					}
				}
			}
		} catch (IOException e) {

			boolean isZipped = false;
			ZipInputStream test = null;
			try {
				test = new ZipInputStream(new FileInputStream(file));
				isZipped = test.getNextEntry() != null;
			} catch(IOException ioe) {
			} finally {
				if( test != null ) {
					try {
						test.close();
					} catch(IOException ioe) {
						// Ignore
					}
				}
			}
			
			String msg = "Error opening zip file " + file.getAbsolutePath();
			if( !isZipped) {
				msg += ";  file may not be a properly formated zip file.";
			}
			return new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, msg, e);
		} finally {
			if (zipFile != null) {
				try {
					zipFile.close();
				} catch (IOException e) {
					// ignore
				}
			}
			monitor.done();
		}
		discoveredRoot = possibleRoot;
		return Status.OK_STATUS;
	}

	
	private void createEntry(IProgressMonitor monitor, ZipFile zipFile,
			ZipEntry entry, File entryFile) throws IOException,
			FileNotFoundException {
		monitor.setTaskName(EXTRACTING + entry.getName());
		if (entry.isDirectory()) {
			entryFile.mkdirs();
		} else {
			entryFile.getParentFile().mkdirs();
			InputStream in = null;
			OutputStream out = null;
			try {
				in = zipFile.getInputStream(entry);
				out = new FileOutputStream(entryFile);
				copy(in, out);
			} finally {
				if (in != null) {
					try {
						in.close();
					} catch (Exception e) {
						// ignore
					}
				}
				if (out != null) {
					try {
						out.close();
					} catch (Exception e) {
						// ignore
					}
				}
			}
		}
	}
	
	private void copy(InputStream in, OutputStream out) throws IOException {
		byte[] buffer = new byte[16 * 1024];
		int len;
		while ((len = in.read(buffer)) >= 0) {
			out.write(buffer, 0, len);
		}
	}
	
	

	/* 
	 * Discover the new root folder of the extracted runtime.
	 */
	public String getRoot(IProgressMonitor monitor) throws CoreException {
		// IF we found a root during the extract, use that.
		if( discoveredRoot != null ) 
			return discoveredRoot;
		if( rootEntryImpossible)
			return null;
		
		monitor.beginTask("Locating root folder", 100);
		ZipFile zipFile = null;
		String root = null;
		try {
			zipFile = new ZipFile(file);
			Enumeration<? extends ZipEntry> entries = zipFile.entries();
			while (entries.hasMoreElements()) {
				if (monitor.isCanceled()) {
					return null;
				}
				ZipEntry entry = (ZipEntry) entries.nextElement();
				String entryName = entry.getName();
				if (entryName == null || entryName.isEmpty() 
						|| entryName.startsWith(SEPARATOR) || entryName.indexOf(SEPARATOR) == -1) {
					return null;
				}
				String directory = entryName.substring(0, entryName.indexOf(SEPARATOR));
				if (root == null) {
					root = directory;
					continue;
				}
				if (!directory.equals(root)) {
					return null;
				}
			}
		} catch(IOException ioe) {
			Status s = new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, ioe.getLocalizedMessage(), ioe);
			throw new CoreException(s);
		} finally {
			if (zipFile != null) {
				try {
					zipFile.close();
				} catch (IOException e) {
					// ignore
				}
			}
			monitor.worked(100);
			monitor.done();
		}
		return root;
	}
	
}
