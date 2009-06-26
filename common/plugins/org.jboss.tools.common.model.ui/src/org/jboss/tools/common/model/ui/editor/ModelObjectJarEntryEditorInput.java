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
package org.jboss.tools.common.model.ui.editor;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IStorageEditorInput;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ModelObjectJarEntryEditorInput extends ModelObjectStorageEditorInput {
	String jarFile;
	String jarEntry;

	public ModelObjectJarEntryEditorInput(XModelObject object, String jarFile, String jarEntry) {
		super(object);
		this.jarFile = jarFile;
		this.jarEntry = jarEntry;
	}
	
	public String getJarFile() {
		return jarFile;
	}
	
	public String getJarEntry() {
		return jarEntry;
	}

	public boolean equals(Object o) {
		if(o instanceof IStorageEditorInput) {
			IStorageEditorInput input = (IStorageEditorInput)o;
			IStorage storage = null;
			try {
				storage = input.getStorage();
			} catch (CoreException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
			String s = (storage == null) ? "" : storage.toString(); //$NON-NLS-1$
			if(jarEntryFileToString().equals(s)) return true;
		}
		return super.equals(o);
	}
	
	//see
	public String jarEntryFileToString() {
		return "JarEntryFile[" + jarFile+"::" + jarEntry+"]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

}
