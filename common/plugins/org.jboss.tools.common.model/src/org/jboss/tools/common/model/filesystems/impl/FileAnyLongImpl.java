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
package org.jboss.tools.common.model.filesystems.impl;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

import org.jboss.tools.common.util.FileUtil;

public class FileAnyLongImpl extends FileAnyImpl {
	private static final long serialVersionUID = 1L;

	public String getAsText() {
		File file = _getFile();
		return (!file.isFile()) ? "" : FileUtil.readFile(file);
	}

	public void edit(String body) {
		File file = _getFile();
		if(file.isFile() || !file.exists()) {
			FileUtil.writeFile(file, body);
			if(getParent() instanceof FolderImpl) {
				FolderImpl folder = (FolderImpl)getParent();
				IFile ef = folder.getChildFile(file.getName());
				if(ef != null) {
					try {
						ef.refreshLocal(IResource.DEPTH_ZERO, null);
					} catch (Exception e) {
						//ignore
					}
				}
			}
		}		
	}
	
	private File _getFile() {
		String f = get("_file");
		return (f == null) ? null : new File(f);
	}

}
