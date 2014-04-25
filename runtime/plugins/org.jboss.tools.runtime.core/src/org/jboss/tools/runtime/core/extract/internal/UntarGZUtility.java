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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.extract.IOverwrite;

public class UntarGZUtility extends UntarUtility {
	
	public UntarGZUtility(File file) {
		super(file);
	}
	
	@Override
	public IStatus extract(File destination, IOverwrite overwriteQuery, IProgressMonitor progress) throws CoreException {
		
		String name = file.getName();
		progress.beginTask(NLS.bind("Extracting {0}", name), 1000);
		
		IPath path = new Path(destination.getAbsolutePath());
		int slashIdx = name.lastIndexOf('/');
		if (slashIdx >= 0)
			name = name.substring(slashIdx + 1);
		FileInputStream in = null;
		try {
			in = new FileInputStream(file);
			File tarFile = File.createTempFile("runtime", ".tar");
			tarFile.deleteOnExit();
			String tarName = name;
			if (slashIdx >= 0)
				tarName = name.substring(0, name.length() - 3);
			
			progress.subTask(NLS.bind("Extracting {0}", tarName));
			int tempSize = Integer.MAX_VALUE;
			if (file.length() < Integer.MAX_VALUE)
				tempSize = (int)file.length();
			
			ungzip(in, tarFile, new SubProgressMonitor(progress, 500), tempSize);
			if (!progress.isCanceled()) {
				in = new FileInputStream(tarFile);
				untar(in, path, new SubProgressMonitor(progress, 500));
			}
		} catch (Exception e) {
			throw new CoreException(new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, 0,
				NLS.bind("Error extracting runtime {0}", e.getLocalizedMessage()), e));
		} finally {
			try {
				if (in != null)
					in.close();
			} catch (IOException e) {
				// ignore
			}
			progress.done();
		}
		return Status.OK_STATUS;
	}
	
	protected void ungzip(InputStream in, File tarFile, IProgressMonitor monitor, int size) throws IOException {
		GZIPInputStream gzin = null;
		FileOutputStream fout = null;
		try {
			gzin = new GZIPInputStream(in);
			fout = new FileOutputStream(tarFile);
			copyWithSize(gzin, fout, monitor, size);
		} finally {
			if (gzin != null) {
				try {
					gzin.close();
				} catch (IOException e) {
					// ignore
				}
				if (fout != null) {
					try {
						fout.close();
					} catch (IOException e) {
						// ignore
					}
				}
			}
		}
	}
	
	
}
