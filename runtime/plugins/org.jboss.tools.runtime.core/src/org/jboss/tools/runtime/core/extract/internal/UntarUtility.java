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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.jboss.tools.runtime.core.extract.internal.xpl.TarEntry;
import org.jboss.tools.runtime.core.extract.internal.xpl.TarInputStream;

public class UntarUtility implements IExtractUtility {
	private static final String SEPARATOR = "/"; //$NON-NLS-1$

	protected File file;
	private String discoveredRoot = null;
	private boolean rootEntryImpossible = false;

	public UntarUtility(File file) {
		this.file = file;
	}
	
	private byte[] BUFFER = null;
	
	public IStatus extract(File destination, IOverwrite overwriteQuery, IProgressMonitor monitor) throws CoreException {
		FileInputStream in = null;
		try {
			in = new FileInputStream(file);
			untar(in, new Path(destination.getAbsolutePath()), monitor);
		} catch(IOException ioe) {
			throw new CoreException(new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, 0,
					NLS.bind("Error extracting runtime", ioe.getLocalizedMessage()), ioe));
		} finally {
			try {
				if (in != null)
					in.close();
			} catch (IOException e) {
				// ignore
			}
			monitor.done();
		}
		return null;
	}
	

	protected void untar(InputStream in, IPath path, IProgressMonitor monitor) throws IOException {
		String possibleRoot = null;

		int fileCnt = -1;
		SubMonitor progress = SubMonitor.convert(monitor, (fileCnt > 0) ? fileCnt : 500);
		String archivePath = null;
		BufferedInputStream bin = new BufferedInputStream(in);
		TarInputStream zin = new TarInputStream(bin);
		TarEntry entry = zin.getNextEntry();
		while (entry != null) {
			String name = entry.getName();
			progress.subTask(NLS.bind("Uncompressing {0}", name));
			if (archivePath != null && name.startsWith(archivePath)) {
				name = name.substring(archivePath.length());
				if (name.length() > 1)
					name = name.substring(1);
			}
			
			if (name != null && name.length() > 0) {
				File entryFile = path.append(name).toFile();
				if (entry.getFileType() == TarEntry.DIRECTORY)
					entryFile.mkdirs();
				else {
					File dir = path.append(name).removeLastSegments(1).toFile();
					if (!dir.exists())
						dir.mkdirs();
					
					if (entryFile.isDirectory()) {
						RuntimeCoreActivator.pluginLog().logError(NLS.bind("Invalid archive, {0} is a directory and a file entry at the same time.", entryFile.getPath())); //$NON-NLS-1$
						entry = zin.getNextEntry();
						continue;
					}
					
					FileOutputStream fout = new FileOutputStream(entryFile);
					copyWithSize(zin, fout, progress.newChild(1), (int)entry.getSize());
					fout.close();
					if (fileCnt <= 0)
						progress.setWorkRemaining(500);
				}
			}
			
			// Lets check for a possible root, to avoid scanning the archive again later
			if( !rootEntryImpossible && discoveredRoot == null) {
				// Check for a root
				if (name == null || name.isEmpty() || name.startsWith(SEPARATOR) || name.indexOf(SEPARATOR) == -1) {
					rootEntryImpossible = true;
					possibleRoot = null;
				} else {
					String directory = name.substring(0, name.indexOf(SEPARATOR));
					if (possibleRoot == null) {
						possibleRoot = directory;
					} else if (!directory.equals(possibleRoot)) {
						rootEntryImpossible = true;
						possibleRoot = null;
					}
				}
			}
			
			entry = zin.getNextEntry();
		}
		this.discoveredRoot = possibleRoot;
		zin.close();
	}
	
	protected void copyWithSize(InputStream in, OutputStream out, IProgressMonitor monitor, int size) throws IOException {
		if (BUFFER == null)
			BUFFER = new byte[8192];
		SubMonitor progress = SubMonitor.convert(monitor, size);
		int r = in.read(BUFFER);
		while (r >= 0) {
			out.write(BUFFER, 0, r);
			progress.worked(r);
			r = in.read(BUFFER);
		}
	}
	
	public String getRoot(IProgressMonitor monitor) throws CoreException {
		// IF we found a root during the extract, use that.
		if( discoveredRoot != null ) 
			return discoveredRoot;
		if( rootEntryImpossible)
			return null;
		// We don't have the .tar file anymore, so can't analyze it. 
		return null;
	}
}
