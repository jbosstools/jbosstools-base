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
import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.runtime.core.extract.IOverwrite;

/**
 * An internal interface for an abstraction that can extract a given
 * compressed file and be aware of what the new root is.
 * This object should already know what file is to be extracted
 * via its constructor or other setter mechanism.
 */
public interface IExtractUtility {
	/**
	 * Extract to the given destination
	 * @param destination
	 * @param overwriteQuery  A query mechanism to verify whether to overwrite files
	 * @param monitor
	 * @return
	 * @throws CoreException
	 */
	public IStatus extract(File destination, 
			IOverwrite overwriteQuery, IProgressMonitor monitor) throws CoreException;
	
	/**
	 * Get the root folder inside the zip, if available, or null. 
	 * @param monitor
	 * @return
	 * @throws IOException
	 */
	public String getRoot(IProgressMonitor monitor) throws IOException;

}
