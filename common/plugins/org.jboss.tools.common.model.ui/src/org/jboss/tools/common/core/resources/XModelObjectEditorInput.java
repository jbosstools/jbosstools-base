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
package org.jboss.tools.common.core.resources;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URI;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJarEntryResource;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.eclipse.ui.internal.part.NullEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.*;

public class XModelObjectEditorInput extends FileEditorInput implements IModelObjectEditorInput {
	private XModelObject object;
	private XModelObjectCache cache;
	
	private XModelObjectEditorInput(XModelObject object) {
		super(getFileByObject(object));
		this.object = object;
		cache = new XModelObjectCache(object);
	}

	/**
	 * Returns true if XModelObject corresponding to the file is replaced.
	 * 
	 * @return
	 */
	public boolean updateXModelObject() {
		IFile file = getFile();
		XModelObject o = EclipseResourceUtil.createObjectForResource(file);
		if(o != null && o != object) {
			object = o;
			cache = new XModelObjectCache(object);
			return true;
		}
		return false;
	}
	
	private static XModelObject getMainObject(XModelObject object) {
		if(object instanceof FileAnyAuxiliaryImpl) {
			XModelObject main = ((FileAnyAuxiliaryImpl)object).getMainObject();
			if(main != null) object = main;
		}
		return object;
	}
	
	public static IModelObjectEditorInput createInstance(XModelObject object) {
		object = getMainObject(object);
		IFile f = getFileByObject(object);
		if(f == null && object instanceof FileAnyImpl) { 
			XModelObject p = object.getParent();
			if(p instanceof FolderImpl) {
				String path = ((FileAnyImpl)object).getAbsolutePath();
				if(path != null) {
					return new ModelObjectLocationEditorInput(object, new Path(path));
				}
			}
			return new ModelObjectStorageEditorInput(object);
		}
		return (f == null) ? null : new XModelObjectEditorInput(object);
	}

	public XModelObject getXModelObject() {
		XModelObject o = cache.getObject();
		if(o != null) object = o;
		return o;
	}

	public boolean exists() {
		IFile f = getFile();
		return f != null && f.exists();
	}

	public ImageDescriptor getImageDescriptor()	{
		return null;
	}

	public String getName()	{
		return object.getPresentationString();
	}

	public String getToolTipText() {
		IFile f = (IFile)EclipseResourceUtil.getResource(object);
		if(f != null && f.exists()) return f.getFullPath().makeRelative().toString();
		return object.getPresentationString();
	}

	public Object getAdapter(Class adapter)	{
		if(adapter == XModelObject.class) return getXModelObject();
		Object result = null;
		if (IFile.class.isAssignableFrom(adapter)) {
			result = EclipseResourceUtil.getResource(object);
			if (result != null && !adapter.isAssignableFrom(result.getClass())) result = null;
		} else {
			result = super.getAdapter(adapter);
		}
		return result; 
	}
	
	public boolean equals(Object o)	{
		getXModelObject();
		if (this == o) return true;
		if (o instanceof FileEditorInput) {
			if(getFile() == null) return false;
			FileEditorInput other = (FileEditorInput)o;
			IFile f1 = getFile();
			IFile f2 = other.getFile();
			if(f1 == null || f2 == null) return f1 == f2;
			if(f1.equals(f2)) return true;
			IPath loc1 = f1.getLocation();
			IPath loc2 = f2.getLocation();
			if(loc1 == null || loc2 == null) return loc1 == loc2;
			return loc1.equals(loc2);
		}
		if(o instanceof XModelObjectEditorInput) {
			return object.equals(((XModelObjectEditorInput)o).getXModelObject());
		}
		return false;
	}

	public IStorage getStorage() {
		return getFile();
	}

	public IFile getFile() {
		return super.getFile();
	}
	
	public boolean isFileChanged() {
		XModelObject o = getXModelObject();
		IFile f1 = super.getFile();
		if (o == null || o.getPath() == null) return false;
		IFile f2 = getFileByObject(o);
		return !f1.equals(f2);
	}
	
	public void synchronize() {
		XModelObject o = getXModelObject();
		IFile f1 = super.getFile();
		if (o == null || o.getPath() == null) return;
		IFile f2 = getFileByObject(o);
		if(!f1.equals(f2) && f2 != null) hackSetFile(f2);
	}
	
	private void hackSetFile(IFile f) {
		try {
			Field field = FileEditorInput.class.getDeclaredField("file"); //$NON-NLS-1$
			field.setAccessible(true);
			field.set(this, f);
		} catch (NoSuchFieldException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalAccessException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}
	
	private static IFile getFileByObject(XModelObject object) {
		return (IFile)EclipseResourceUtil.getResource(object);
	}
	
	public static IEditorInput checkInput(IEditorInput input) {
		if(input instanceof IModelObjectEditorInput) return input;
		if(input instanceof ILocationProvider) return convertExternalInput((ILocationProvider)input);
		if(input instanceof IFileEditorInput) return convertFileInput((IFileEditorInput)input);
		if(input instanceof IStorageEditorInput) return convertStorageEditorInput((IStorageEditorInput)input);
		if(input instanceof IURIEditorInput) {
			URI uri = ((IURIEditorInput)input).getURI();
			String f = uri.getPath();
			XModelObject o = EclipseResourceUtil.createObjectForLocation(f);
			return (o == null || o.getFileType() != XModelObject.FILE) ? (IEditorInput)input : new ModelObjectLocationEditorInput(getMainObject(o), new Path(f));
		}
		return input;
	}
	
	private static IEditorInput convertFileInput(IFileEditorInput input) {
		IFileEditorInput fi = (IFileEditorInput)input;
		IFile f = fi.getFile();
		if(f != null && !f.isSynchronized(IResource.DEPTH_INFINITE)) {
			try {
				f.refreshLocal(IResource.DEPTH_INFINITE, null);
			} catch (CoreException e) {
				//ignore
			}
		}
		XModelObject o = EclipseResourceUtil.getObjectByResource(f);
		if(o == null) {
			o = EclipseResourceUtil.createObjectForResource(f);
		}
		return (o == null || o.getFileType() != XModelObject.FILE) ? input : new XModelObjectEditorInput(getMainObject(o));
	}
	
	private static IEditorInput convertExternalInput(ILocationProvider input) {
		XModelObject o = EclipseResourceUtil.createObjectForLocation(input.getPath(input).toString());
		return (o == null || o.getFileType() != XModelObject.FILE) ? (IEditorInput)input : new ModelObjectLocationEditorInput(getMainObject(o), input.getPath(input));
	}
	
	private static IEditorInput convertStorageEditorInput(IStorageEditorInput input) {
		if(input instanceof JarEntryEditorInput) {
			IProject project = null;
			JarEntryEditorInput j = (JarEntryEditorInput)input;
			if(j.getStorage() instanceof IJarEntryResource) {
				IJarEntryResource file = (IJarEntryResource)j.getStorage();
				String jarFile = file.getPackageFragmentRoot().getPath().toString();
				if(file.getPackageFragmentRoot().getResource() != null) {
					jarFile = file.getPackageFragmentRoot().getResource().getLocation().toString();
				}
				String entry = file.getName();
				IJarEntryResource r = file;
				while(r != null && r.getParent() instanceof IJarEntryResource) {
					r = (IJarEntryResource)r.getParent();
					entry = r.getName() + "/" + entry;
				}
				if(r != null && r.getParent() instanceof IPackageFragment) {
					IPackageFragment pf = (IPackageFragment)r.getParent();
					IJavaProject jp = pf.getJavaProject();
					if(jp != null) project = jp.getProject();
					while(pf != null) {
						String p = pf.getElementName();
						entry = p + "/" + entry;
						if(pf.getParent() instanceof IPackageFragment) {
							pf = (IPackageFragment)pf.getParent();
						} else {
							pf = null;
						}
					}
				} else if(r != null && r.getPackageFragmentRoot() != null) {
					IPackageFragmentRoot root = r.getPackageFragmentRoot();
					if(root.getJavaProject() != null) {
						project = root.getJavaProject().getProject();
					}
				}
				IEditorInput result = createJarEntryEditorInput(project, jarFile, entry);
				if(result != null) return result;
			}
		}
		String[] entryInfo = parseJarEntryFileInput(input);
		if(entryInfo == null) return input;
		String jarFile = entryInfo[0];
		String entry = entryInfo[1];
		IEditorInput result = createJarEntryEditorInput(null, jarFile, entry);
		return (result == null || result instanceof NullEditorInput) ? input : result;
	}

	public static String[] parseJarEntryFileInput(IStorageEditorInput input) {
		IStorage storage = null;
		try {
			storage = input.getStorage();
		} catch (CoreException e) {
			// ignore
		}
		return storage == null ? null : parseJarEntryFile(storage);
	}

	public static String[] parseJarEntryFile(IStorage storage) {
		if(storage == null) return null;
		String s = storage.toString();
		if(!s.startsWith("JarEntryFile[")) return null; //$NON-NLS-1$
		s = s.substring("JarEntryFile[".length()); //$NON-NLS-1$
		int i = s.indexOf("::"); //$NON-NLS-1$
		if(i < 0) return null;
		String jarFile = s.substring(0, i);
		String entry = storage.getFullPath().toString();
		return new String[]{jarFile, entry};
	}
	
	public static XModelObject getJarEntryObject(IProject p, String jarFile, String entry) {
		File jf = new File(jarFile);
		try {
			jf = new File(new File(jarFile).getCanonicalPath());
		} catch (IOException e) {
			ModelPlugin.getDefault().logError(e);
		}
		if(p == null) {
			IFile f = EclipseResourceUtil.getFile(jarFile);
			if(f == null) return null;
			p = f.getProject();
		}
		if(p == null) return null;
		IModelNature n = EclipseResourceUtil.getModelNature(p);
		XModel model = null;
		if(n != null) {
			model = n.getModel();
		} else {
			XModelObject o = EclipseResourceUtil.createObjectForResource(p);
			if(o != null) model = o.getModel();
		}
		if(model == null) return null;
		XModelObject[] fs = FileSystemsHelper.getFileSystems(model).getChildren();
		for (XModelObject s: fs) {
			String loc = Paths.expand(s.get(XModelObjectConstants.ATTR_NAME_LOCATION), model.getProperties());
			if(new File(loc).equals(jf)) {
				XModelObject result = s.getChildByPath(entry);
				if(result == null && entry != null) {
					int q = entry.indexOf('/');
					int d = entry.indexOf('.');
					if(q > d && d >= 0) {
						String entry1 = entry.substring(0, q).replace('.', '/') + entry.substring(q);
						result = s.getChildByPath(entry1);
					}
				}
				if(result != null) return result;
			}
		}
		return (n == null) ? null : n.getModel().getByPath("/" + entry);		 //$NON-NLS-1$
	}
	
	public static IEditorInput createJarEntryEditorInput(IProject project, String jarFile, final String entry) {
		XModelObject o = getJarEntryObject(project, jarFile, entry);
		if(o != null) return new ModelObjectJarEntryEditorInput(o, jarFile, entry);
		return XModelObjectEditorInputFactory.createNullEditorInput(entry);
	}
	
	public String getFactoryId() {
		return XModelObjectEditorInputFactory.getFactoryId();
	}

	public void saveState(IMemento memento) {
		XModelObjectEditorInputFactory.saveState(memento, this);
	}
	
	public void revalidate() {
		IFile f = getFile();
		if(f == null || f.equals(super.getFile())) return;
		try {
			Field field = FileEditorInput.class.getDeclaredField("file"); //$NON-NLS-1$
			field.setAccessible(true);
			field.set(this, f);
		} catch (NoSuchFieldException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalAccessException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

}
