/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;

/**
 * @author Alexey Kazakov
 */
public class ValidationResourceRegister {
	private Set<IFile> removedFiles = new HashSet<IFile>();
	private Set<IFile> registeredResources = new HashSet<IFile>();
	private boolean obsolete = false;

	public void clear() {
		synchronized (removedFiles) {
			removedFiles.clear();
		}
		synchronized (registeredResources) {
			registeredResources.clear();
		}
		obsolete = true;
	}

	public Set<IFile> getRemovedFiles() {
		return removedFiles;
	}

	public void addRemovedFile(IFile file) {
		removedFiles.add(file);
	}

	public Set<IFile> getRegisteredFiles() {
		Set<IFile> copy = new HashSet<IFile>();
		synchronized (registeredResources) {
			copy.addAll(registeredResources);
		}
		return copy;
	}

	public void registerFile(IFile file) {
		obsolete = false;
		synchronized (registeredResources) {
			registeredResources.add(file);
		}
	}

	/**
	 * @return the obsolete
	 */
	public boolean isObsolete() {
		return obsolete;
	}
}