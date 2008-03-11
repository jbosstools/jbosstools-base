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
package org.jboss.tools.common.model.ui.wizard.newfile;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.IStructuredSelection;

import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.files.handlers.CreateFileSupport;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public abstract class NewFileContextEx extends NewFileContext {

	protected SpecialWizardSupport createSupport() {
		CreateFileSupport support = new CreateFileSupport();
		return support;
	}

	/*
	 * Method must be overriden
	 */	
	protected String getActionPath() {
		return null;
	}
		
	public void setSelection(IStructuredSelection selection) {
		errorMesage = null;
		resource = null;
		folder = null;
		if(selection.isEmpty() || selection.size() > 1) return;
		Object o = selection.getFirstElement();
		if(o instanceof IResource) {
			resource = (IResource)o;
		} else if(o instanceof IAdaptable) {
			resource = (IResource)((IAdaptable)o).getAdapter(IResource.class);
		}
		if(resource != null && !resource.getProject().isOpen()) {
			errorMesage = "Project is closed";
			return;
		}
		
		folder = EclipseResourceUtil.getObjectByResource(resource);
		if(folder == null) {
			folder = EclipseResourceUtil.createObjectForResource(resource);
		}
		if(folder != null) {
			XModel model = folder.getModel();
			if(folder.getFileType() > XModelObject.FILE) {
			} else if(folder.getFileType() == XModelObject.NONE) {
				if(folder == FileSystemsHelper.getFileSystems(model) && resource != null && resource == resource.getProject()) {
					folder = EclipseResourceUtil.findFileSystem(resource, folder.getModel());
					if(folder == null) folder = getDefaultFolder(model);
				} else {
					folder = FileSystemsHelper.getWebInf(model);
				}
			} else if(folder.getFileType() == XModelObject.FILE) {
				folder = folder.getParent();
			}
		}
		if(!(folder instanceof FolderImpl)) folder = null;
		if(folder == null) {
			errorMesage = "Cannot find resource " + resource.getLocation().toString();
		} else {
			support.setActionData(action, action.getEntityData(folder), folder, null);
		}			
	}
	
	protected XModelObject getDefaultFolder(XModel model) {
		return FileSystemsHelper.getWebInf(model);
	}
	
}
