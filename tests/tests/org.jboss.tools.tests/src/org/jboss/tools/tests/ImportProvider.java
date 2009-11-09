/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.tests;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.eclipse.ui.wizards.datatransfer.IImportStructureProvider;

/**
 * @author sdzmitrovich
 * 
 */
public class ImportProvider implements IImportStructureProvider {

	/**
	 * list of files which will not be imported
	 */
	List<String> unimportedFiles = new ArrayList<String>();

	/**
	 * Creates an instance of <code>ImportProvider</code>.
	 */
	public ImportProvider() {
		super();
	}

	/*
	 * (non-Javadoc) Method declared on IImportStructureProvider
	 */
	public List<File> getChildren(Object element) {
		File folder = (File) element;
		String[] children = folder.list();
		int childrenLength = children == null ? 0 : children.length;
		List<File> result = new ArrayList<File>(childrenLength);

		for (int i = 0; i < childrenLength; i++) {
			if (!unimportedFiles.contains(children[i]))
				result.add(new File(folder, children[i]));
		}

		return result;
	}

	/*
	 * (non-Javadoc) Method declared on IImportStructureProvider
	 */
	public InputStream getContents(Object element) {
		try {
			return new FileInputStream((File) element);
		} catch (FileNotFoundException e) {
			IDEWorkbenchPlugin.log(e.getLocalizedMessage(), e);
			return null;
		}
	}

	/*
	 * (non-Javadoc) Method declared on IImportStructureProvider
	 */
	public String getFullPath(Object element) {
		return ((File) element).getPath();
	}

	/*
	 * (non-Javadoc) Method declared on IImportStructureProvider
	 */
	public String getLabel(Object element) {

		// Get the name - if it is empty then return the path as it is a file
		// root
		File file = (File) element;
		String name = file.getName();
		if (name.length() == 0) {
			return file.getPath();
		}
		return name;
	}

	/*
	 * (non-Javadoc) Method declared on IImportStructureProvider
	 */
	public boolean isFolder(Object element) {
		return ((File) element).isDirectory();
	}

	public List<String> getUnimportedFiles() {
		return unimportedFiles;
	}

	public void setUnimportedFiles(List<String> unimportedFiles) {
		this.unimportedFiles = unimportedFiles;
	}

}
