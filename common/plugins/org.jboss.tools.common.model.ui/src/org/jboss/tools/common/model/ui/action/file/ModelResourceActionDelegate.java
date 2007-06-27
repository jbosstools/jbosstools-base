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
package org.jboss.tools.common.model.ui.action.file;

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.jdt.core.*;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ModelResourceActionDelegate implements IObjectActionDelegate, IWorkbenchWindowActionDelegate {
	IResource resource = null;
	protected XModelObject object;
	protected List<XModelObject> objects = new ArrayList<XModelObject>();

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}

	public void run(IAction action) {
		if (action.isEnabled()) doRun();
	}

	private void doRun() {
		Properties p = new Properties();
		initProperties(p);
		if(objects.size() < 2) {
			XActionInvoker.invoke(getActionPath(), object, p);
		} else {
			XModelObject[] os = objects.toArray(new XModelObject[0]);
			XActionInvoker.invoke(getActionPath(), object, os, p);
		}
	}
	
	protected void initProperties(Properties p) {
		
	}
	
	protected String getActionPath() {
		return null;
	}
	
	public void selectionChanged(IAction action, ISelection selection) {
		resource = null;
		object = null;
		objects.clear();
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection structuredSelection = (IStructuredSelection)selection; 
			if (structuredSelection.size() == 1) {
				Object o = structuredSelection.getFirstElement();
				process(o);
				if(object != null) {
					action.setEnabled(compureEnabled());
				} else {
					object = getObjectByResource(resource);
					action.setEnabled(compureEnabled());
				}
			} else if(structuredSelection.size() > 1) {
				Iterator it = structuredSelection.iterator();
				while(it.hasNext()) {
					Object o = it.next();
					object = null;
					process(o);
					if(object == null && resource != null) {
						object = getObjectByResource(resource);
					}
					if(object != null) {
						objects.add(object);
					} else {
						resource = null;
						objects.clear();
						action.setEnabled(compureEnabled());
						return;
					}
				}
				action.setEnabled(compureEnabled());
			}
		}
	}
	
	private void process(Object o) {
		if(checkModelObject(o)) {}
		else if (checkFile(o)) {} 
		else if(checkProject(o)) {}
		else if(checkFolder(o)) {}
	}
	
	XModelObject getObjectByResource(IResource resource) {
		XModelObject object = null;
		if(resource != null && !isRelevantProject(resource.getProject())) {
			resource = null;
		}
		if(resource == null) {
			object = null;
		} else if(resource instanceof IProject) {
			IModelNature n = EclipseResourceUtil.getModelNature((IProject)resource);
			if(n != null) {
				object = FileSystemsHelper.getFileSystems(n.getModel());
			} else {
				object = null;
			}
		} else {
			object = EclipseResourceUtil.getObjectByResource(resource);
			if(object == null) object = EclipseResourceUtil.createObjectForResource(resource);
			if(object != null) checkModelObject(object);
		}
		return object;
	}
	
	protected boolean isRelevantProject(IProject project) {
		return true;
	}
	
	protected boolean checkModelObject(Object object) {
		if(object instanceof XModelObject) {
			this.object = (XModelObject)object;
			return true;
		}
		return false;
	}
	
	protected boolean checkFile(Object object) {
		if(object instanceof IFile) {
			if(isSupportingImplementation(IFile.class)) {	
				resource = (IFile)object;					
			} else if(isWindowAction && isSupportingImplementation(IProject.class)) {
				resource = ((IFile)object).getProject();
			}
		} else if(object instanceof IClassFile) {
			if(isSupportingImplementation(IFile.class)) {	
				try {
					resource = ((IClassFile)object).getCorrespondingResource();
				} catch (Exception e) {}
			} else if(isWindowAction && isSupportingImplementation(IJavaProject.class)) {
				object = ((IClassFile)object).getJavaProject();
			}
		} else {
			return false;
		}
		return true;
	}

	protected boolean checkFolder(Object object) {
		if(object instanceof IFolder) {
			if(isSupportingImplementation(IFolder.class)) {	
				resource = (IFolder)object;					
			} else if(isWindowAction && isSupportingImplementation(IProject.class)) {
				resource = ((IResource)object).getProject();
			}
		} else {
			return false;
		}
		return true;
	}	
	
	protected boolean checkProject(Object object) {
		IProject project = null;
		if(object instanceof IProject) {
			if(!isSupportingImplementation(IProject.class)) return true;
			project = (IProject)object;
		} else if(object instanceof IJavaProject) {
			if(!isSupportingImplementation(IJavaProject.class)) return true;
			project = ((IJavaProject)object).getProject();
		} else {
			return false;
		}
		if (project != null && (!project.isOpen() || !hasModelNature(project)))
			project = null;
		resource = project;
		return true;
	}
	
	protected boolean hasModelNature(IProject project) {
		String[] s = EclipseResourceUtil.getModelNatureNames();
		for (int i = 0; i < s.length; i++) {
			try {
				if(project.hasNature(s[i])) return true;
			} catch (Exception e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		return false;
	}
	
	protected boolean isSupportingImplementation(Class cls) {
		return (cls == IFile.class || cls == XModelObject.class);
	}
	
	protected boolean compureEnabled() {
		boolean enabled = (object != null);
		if(enabled) {
			XAction a = object.getModelEntity().getActionList().getAction(getActionPath());
			enabled = a != null 
			        && ((objects.size() < 2) ? a.isEnabled(object) 
					    : a.isEnabled(object, objects.toArray(new XModelObject[0])));
		} 
		return enabled; 
	}

	public void dispose() {}
	
	protected boolean isWindowAction;

	public void init(IWorkbenchWindow window) {
		isWindowAction = true;
	}

}