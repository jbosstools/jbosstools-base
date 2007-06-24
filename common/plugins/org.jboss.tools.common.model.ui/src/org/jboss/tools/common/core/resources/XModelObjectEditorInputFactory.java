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
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.FileSystemImpl;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.ui.editor.ModelObjectEditorInput;
import org.jboss.tools.common.model.ui.editor.ModelObjectJarEntryEditorInput;
import org.jboss.tools.common.model.ui.editor.ModelObjectLocationEditorInput;
import org.jboss.tools.common.model.ui.editor.ModelObjectStorageEditorInput;

public class XModelObjectEditorInputFactory implements IElementFactory {
	private static final String ID_FACTORY = 
		"org.jboss.tools.common.core.resources.XModelObjectEditorInputFactory";
	private static final String TAG_PATH = "path";
	private static final String TAG_PROJECT = "project";
	private static final String TAG_FILE_LOCATION = "filepath";
	private static final String TAG_ENTRY = "entry";

	public IAdaptable createElement(IMemento memento) {
		String projectPath = memento.getString(TAG_PROJECT);
		if(projectPath != null) return createStorageElement(memento);
		String fileLocation = memento.getString(TAG_FILE_LOCATION);
		if(fileLocation != null) return createStorageElement(memento);
		String fileName = memento.getString(TAG_PATH);
		if (fileName == null) return null;
		IResource res = ResourcesPlugin.getWorkspace().getRoot().findMember(new Path(fileName));
		if (res instanceof IFile) {
			XModelObject o = EclipseResourceUtil.getObjectByResource(res);
			return (o != null) ? (IAdaptable)XModelObjectEditorInput.createInstance(o)
			       : (IAdaptable)new FileEditorInput((IFile)res);
		} else {
			res = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(fileName));
			return (IAdaptable)new FileEditorInput((IFile)res);
		}
	}
	
	private IAdaptable createStorageElement(IMemento memento) {
		String projectPath = memento.getString(TAG_PROJECT);
		XModelObject object = null;
		if(projectPath == null) {
			String fileLocation = memento.getString(TAG_FILE_LOCATION);
			if(fileLocation != null) {
				File f = new File(fileLocation);
				if(f.isFile()) {
					object = EclipseResourceUtil.createObjectForLocation(fileLocation);
					return (object == null) ? null : new ModelObjectLocationEditorInput(object, new Path(f.getAbsolutePath()));
				}
			}
		} else if(memento.getString(TAG_ENTRY) != null) {
			String fileLocation = memento.getString(TAG_FILE_LOCATION);
			String entry = memento.getString(TAG_ENTRY);
			return XModelObjectEditorInput.createJarEntryEditorInput(fileLocation, entry);
		} else {
			IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().findMember(new Path(projectPath));
			String objectPath = memento.getString(TAG_PATH);
			IModelNature nature = EclipseResourceUtil.getModelNature(project);
			if(nature == null) {
			} else {
				XModel model = nature.getModel();
				object = model.getByPath(objectPath);
			}
		}
		if(object == null) return null;
		return new ModelObjectStorageEditorInput(object);
	}
	
	public static String getFactoryId() {
		return ID_FACTORY;
	}

	public static void saveState(IMemento memento, IFileEditorInput input) {
		IFile file = input.getFile();
		memento.putString(TAG_PATH, file.getFullPath().toString());
	}
	
	public static void saveState(IMemento memento, ModelObjectEditorInput input) {
		if(input instanceof ModelObjectJarEntryEditorInput) {
			saveState(memento, (ModelObjectJarEntryEditorInput)input);
			return;
		}
		XModelObject o = input.getXModelObject();
		IProject p = EclipseResourceUtil.getProject(o);
		if(p != null) {
			memento.putString(TAG_PROJECT, p.getFullPath().toString());		
			memento.putString(TAG_PATH, o.getPath());
		} else {
			String location = getFileLocation(o);
			if(location != null) memento.putString(TAG_FILE_LOCATION, location);
		}
	}

	public static void saveState(IMemento memento, ModelObjectJarEntryEditorInput input) {
		XModelObject o = input.getXModelObject();
		IProject p = EclipseResourceUtil.getProject(o);
		if(p != null) {
			memento.putString(TAG_PROJECT, p.getFullPath().toString());		
			memento.putString(TAG_FILE_LOCATION, input.getJarFile());
			memento.putString(TAG_ENTRY, input.getJarEntry());
		} else {
			String location = getFileLocation(o);
			if(location != null) memento.putString(TAG_FILE_LOCATION, location);
		}
	}	

	public static String getFileLocation(XModelObject object) {
		if(object == null || object.getFileType() != XModelObject.FILE) return null;
		XModelObject fs = object.getParent();
		while(fs != null && fs.getFileType() != XModelObject.SYSTEM) fs = fs.getParent();
		if(fs instanceof FileSystemImpl) {
			FileSystemImpl fsi = (FileSystemImpl)fs;
			String location = fsi.getAbsoluteLocation() + XModelObjectLoaderUtil.getResourcePath(object);
			return location; 
		}
		return null;
	}

}
