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
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.core.JarEntryResource;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.eclipse.ui.internal.part.NullEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.impl.*;
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
		if(f != null && f.exists()) return f.getLocation().toString();
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
//		XModelObject o = getXModelObject();
//		if (o == null || o.getPath() == null)
			return super.getFile();
//		return getFileByObject(o);
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
			XModelObject o = null;
			o = EclipseResourceUtil.createObjectForLocation(f);
			if(o != null && o.getFileType() != XModelObject.FILE) o = null;
			return (o == null) ? (IEditorInput)input : new ModelObjectLocationEditorInput(getMainObject(o), new Path(f));
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
			if(o != null && o.getFileType() != XModelObject.FILE) o = null;
		}
		return (o == null) ? input : new XModelObjectEditorInput(getMainObject(o));
	}
	
	private static IEditorInput convertExternalInput(ILocationProvider input) {
		XModelObject o = null;
		o = EclipseResourceUtil.createObjectForLocation(input.getPath(input).toString());
		if(o != null && o.getFileType() != XModelObject.FILE) o = null;
		return (o == null) ? (IEditorInput)input : new ModelObjectLocationEditorInput(getMainObject(o), input.getPath(input));
	}
	
	private static IEditorInput convertStorageEditorInput(IStorageEditorInput input) {
		if(input instanceof JarEntryEditorInput) {
			JarEntryEditorInput j = (JarEntryEditorInput)input;
			JarEntryFile file = (JarEntryFile)j.getStorage();
			String jarFile = file.getPackageFragmentRoot().getPath().toString();
			String entry = file.getName();
			JarEntryResource r = file;
			while(r != null && r.getParent() instanceof JarEntryResource) {
				r = (JarEntryResource)r.getParent();
				entry = r.getName() + "/" + entry;
			}
			if(r != null && r.getParent() instanceof IPackageFragment) {
				IPackageFragment pf = (IPackageFragment)r.getParent();
				while(pf != null) {
					String p = pf.getElementName();
					entry = p + "/" + entry;
					if(pf.getParent() instanceof IPackageFragment) {
						pf = (IPackageFragment)pf.getParent();
					} else {
						pf = null;
					}
				}
			}
			IEditorInput result = createJarEntryEditorInput(jarFile, entry);
			if(result != null) return result;
		}
		String[] entryInfo = parseJarEntryFileInput(input);
		if(entryInfo == null) return input;
		String jarFile = entryInfo[0];
		String entry = entryInfo[1];
		IEditorInput result = createJarEntryEditorInput(jarFile, entry);
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
	
	public static XModelObject getJarEntryObject(String jarFile, String entry) {
		IFile f = EclipseResourceUtil.getFile(jarFile);
		if(f == null) return null;
		IProject p = f.getProject();
		IModelNature n = EclipseResourceUtil.getModelNature(p);
		if(n == null) return null;
		XModelObject[] fs = FileSystemsHelper.getFileSystems(n.getModel()).getChildren();
		for (XModelObject s: fs) {
			if(s instanceof JarSystemImpl) {
				JarSystemImpl j = (JarSystemImpl)s;
				String loc = j.getLocation();
				if(new File(loc).equals(new File(jarFile))) {
					XModelObject result = s.getChildByPath(entry);
					if(result != null) return result;
				}
			}
		}
		return (n == null) ? null : n.getModel().getByPath("/" + entry);		 //$NON-NLS-1$
	}
	
	public static IEditorInput createJarEntryEditorInput(String jarFile, final String entry) {
		XModelObject o = getJarEntryObject(jarFile, entry);
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
