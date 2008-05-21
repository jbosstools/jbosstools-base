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

import java.io.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.jboss.tools.common.core.resources.XModelObjectEditorInputFactory;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.ILocationProvider;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class ModelObjectStorageEditorInput extends ModelObjectEditorInput implements IStorageEditorInput {
	
	public ModelObjectStorageEditorInput(XModelObject object) {
		super(object);
	}

	public IStorage getStorage() throws CoreException {
		return storage;
	}
	
	IStorage storage = new Storage();
	
	class Storage implements IStorage {

		public InputStream getContents() throws CoreException {
			ByteArrayInputStream b = null;
			if(object instanceof FileAnyImpl) {
				FileAnyImpl f = (FileAnyImpl)object;
				b = new ByteArrayInputStream(f.getAsText().getBytes());
			} else {
				b = new ByteArrayInputStream(new byte[0]);
			}			
			return b;
		}

		public IPath getFullPath() {
			IProject p = EclipseResourceUtil.getProject(object);
			if(p == null) {
				String location = XModelObjectEditorInputFactory.getFileLocation(object);
				if(location != null) {
					return new Path(location); 
				}
			} else {
				XModelObject f = object;
				while(f != null && f.getFileType() != XModelObject.SYSTEM) f = f.getParent();
				if(f != null) {
					IResource jar = EclipseResourceUtil.getResource(f);
					if(jar != null) return jar.getFullPath();
				}
			}
			String n = p == null ? "" : p.getName();
			return new Path(n + "/" + object.getPath());
		}

		public String getName() {
			return ModelObjectStorageEditorInput.this.getName();
		}

		public boolean isReadOnly() {
			return !isEditable();
		}

		public Object getAdapter(Class adapter) {
			if(adapter == ILocationProvider.class) {
				if(ModelObjectStorageEditorInput.this instanceof ILocationProvider) {
					return (ILocationProvider)ModelObjectStorageEditorInput.this;
				}
			}
			return ModelObjectStorageEditorInput.this.getAdapter(adapter);
		}
	}
	
	protected boolean isEditable() {
		return false;
	}

	public boolean equals(Object o)	{
		if(super.equals(o)) return true;
		if(o instanceof IStorageEditorInput) {
			String[] entryInfo = XModelObjectEditorInput.parseJarEntryFileInput((IStorageEditorInput)o);
			if(entryInfo == null) return false;
			XModelObject mo = XModelObjectEditorInput.getJarEntryObject(entryInfo[0], entryInfo[1]);
			return mo != null && mo.equals(object);
		}
		return false;
	}
}
