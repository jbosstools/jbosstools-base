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
package org.jboss.tools.runtime.core.extract;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.extract.internal.IExtractUtility;
import org.jboss.tools.runtime.core.extract.internal.UntarGZUtility;
import org.jboss.tools.runtime.core.extract.internal.UntarUtility;
import org.jboss.tools.runtime.core.extract.internal.UnzipUtility;

public class ExtractUtility {
	private static final String ZIP_SUFFIX = "zip"; //$NON-NLS-1$
	private static final String TAR_SUFFIX = "tar"; //$NON-NLS-1$
	private static final String TAR_GZ_SUFFIX = "tar.gz"; //$NON-NLS-1$
	
	
	private File file;
	private IExtractUtility util;
	public ExtractUtility(File file) {
		this.file = file;
		String name = file.getName().toLowerCase();
		if( name.endsWith(ZIP_SUFFIX)) {
			util = new UnzipUtility(file);
		} else if( name.endsWith(TAR_SUFFIX)) {
			util = new UntarUtility(file);
		} else if( name.endsWith(TAR_GZ_SUFFIX)) {
			util = new UntarGZUtility(file);
		}
	}
	
	public File getOriginalFile() {
		return file;
	}
	
	public IStatus extract(File destination, IOverwrite overwriteQuery, IProgressMonitor monitor) {
		if( util != null ) {
			try {
				return util.extract(destination, overwriteQuery, monitor);
			} catch(CoreException ce) {
				return new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, "Error extracting file " + file.getAbsolutePath(), ce);
			}
		}
		return new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, "Unable to discover how to extract file " + file.getAbsolutePath());
	}
	
	public String getExtractedRootFolder(IProgressMonitor monitor) throws IOException {
		if( util != null )
			return util.getRoot(monitor);
		return null;
	}
	
}
