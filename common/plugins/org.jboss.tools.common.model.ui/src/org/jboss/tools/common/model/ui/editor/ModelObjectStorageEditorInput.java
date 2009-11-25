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
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.internal.core.JarEntryDirectory;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.core.JarEntryResource;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.jboss.tools.common.core.resources.XModelObjectEditorInputFactory;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.ILocationProvider;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.util.FileUtil;

public class ModelObjectStorageEditorInput extends ModelObjectEditorInput implements IStorageEditorInput {
	JarEntryFile jarEntryFile = null;

	public ModelObjectStorageEditorInput(XModelObject object) {
		super(object);
		jarEntryFile = findJarEntryFile();
	}

	public IStorage getStorage() throws CoreException {
		return jarEntryFile != null ? jarEntryFile : storage;
	}

	JarEntryFile findJarEntryFile() {
		XModelObject o = object;
		JarEntryFile f = null;
		JarEntryResource current = null;
		while(o != null && o.getFileType() != XModelObject.SYSTEM) {
			String part = o.getFileType() == XModelObject.FILE ? FileAnyImpl.toFileName(o) :
				o.getFileType() == XModelObject.FOLDER ? o.getAttributeValue(XModelObjectConstants.ATTR_NAME) : null;
			if(part != null) {
				if(f == null) {
					f = new JarEntryFile(part) {
						public InputStream getContents() throws CoreException {
							return storage.getContents();
						}
					};
					current = f;
				} else {
					if(f == null) return null;
					JarEntryDirectory d = new JarEntryDirectory(part);
					current.setParent(d);
					current = d;
				}
				
			}
			o = o.getParent();
		}
		if(!(o instanceof JarSystemImpl)) return null;
		String file = ((JarSystemImpl)o).getLocation();
		
		try {
			file = new File(file).getCanonicalPath();
		} catch (IOException e) {
			
		}
		
		IFile[] fs = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocationURI(new File(file).toURI());
		if(fs == null || fs.length == 0) return null;
		
        IProject p = fs[0].getProject();        
        IJavaProject jp = EclipseResourceUtil.getJavaProject(p);        
        if(jp == null) return null;
        
        IPackageFragmentRoot root = jp.getPackageFragmentRoot(file);
        if(root == null) return null;
		current.setParent(root);
		
		return f;
	}
	
	IStorage storage = new Storage();
	
	class Storage implements IStorage {

		public InputStream getContents() throws CoreException {
			ByteArrayInputStream b = null;
			if(object instanceof FileAnyImpl) {
				FileAnyImpl f = (FileAnyImpl)object;
				String s = f.getAsText();
				String encoding = FileUtil.getEncoding(s);
				byte[] bs = null;
				if(encoding == null) { 
					bs = s.getBytes();
				} else {
					try {
						bs = s.getBytes(encoding);
					} catch (UnsupportedEncodingException e) {
						bs = s.getBytes();
					}
				}
				b = new ByteArrayInputStream(bs);
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
			String n = p == null ? "" : p.getName(); //$NON-NLS-1$
			return new Path(n + "/" + object.getPath()); //$NON-NLS-1$
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
			try {
				IStorage st = ((IStorageEditorInput)o).getStorage();
				if(jarEntryFile != null && jarEntryFile.equals(st)) return true;
			} catch (CoreException e) {
				//ignore
			}
			String[] entryInfo = XModelObjectEditorInput.parseJarEntryFileInput((IStorageEditorInput)o);
			if(entryInfo == null) return false;
			XModelObject mo = XModelObjectEditorInput.getJarEntryObject(entryInfo[0], entryInfo[1]);
			return mo != null && mo.equals(object);
		}
		return false;
	}
}
