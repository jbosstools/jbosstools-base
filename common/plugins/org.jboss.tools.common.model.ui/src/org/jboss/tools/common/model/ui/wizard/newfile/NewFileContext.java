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

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.viewers.IStructuredSelection;

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class NewFileContext {
	protected IResource resource;
	protected XModelObject folder;
	protected XModelObject fakeFolder;
	protected XAction action;
	protected SpecialWizardSupport support = createSupport();
	protected String errorMesage = null;
	protected String helpkey; 
	static String COMPANY_NAME = "Red Hat";

	public NewFileContext() {}

	public void init() {
		XModel model = ModelUtilities.getPreferenceModel();
		fakeFolder = model.createModelObject("FileFolder", null);
		XActionList list = model.getMetaData().getEntity("FileFolder").getActionList();
		action = list.getAction(getActionPath());
		support.setActionData(action, action.getEntityData(fakeFolder), fakeFolder, null);
		helpkey = "FileFolder_" + action.getName();
	}
	
	protected SpecialWizardSupport createSupport() {
		return new SWS();
	}
	
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
		try {
			if(EclipseResourceUtil.getModelNature(resource.getProject()) == null)
			  errorMesage = "Add Struts Nature to project";
		} catch (Exception e) {
			errorMesage = e.getMessage();
			return;
		}
		
		folder = EclipseResourceUtil.getObjectByResource(resource);
		if(folder != null) {
			if(folder.getFileType() > XModelObject.FILE) {
			} else if(folder.getFileType() == XModelObject.NONE)
			  folder = folder.getModel().getByPath("FileSystems/WEB-INF");
			else if(folder.getFileType() == XModelObject.FILE)
			  folder = folder.getParent();
		}
		if(!(folder instanceof FolderImpl)) folder = null;
		if(folder == null) {
			errorMesage = "Cannot find " + COMPANY_NAME + " model element for resource " + resource.getLocation().toString();
		} else {
			support.setActionData(action, action.getEntityData(folder), folder, null);
		}
	}
	
	class SWS extends SpecialWizardSupport {
		public SWS() {}
		public void action(String name) throws Exception {}
	}
	
	public String validate(Properties data) {
		if(errorMesage != null) return errorMesage;
		support.getValidator(0).validate(data);
		return support.getValidator(0).getErrorMessage();
	}
	
	public void execute() throws Exception {
		Properties p = new Properties();
		if(resource != null) p.put("resource", resource);
		action.executeHandler(support.getTarget(), p);
	}
	
	public boolean update() {
		if(folder == null) return false;
		action.getEntityData(folder);
		return true;
	}
	
	public XModelObject getFakeObject() {
		return fakeFolder;
	}
	
	public SpecialWizardSupport getSupport() {
		return support;
	}
	
	public String getWindowTitle() {
		String title = WizardKeys.getHeader(helpkey);
		return (title == null) ? "" : title;
	}
	public String getTitle() {
		String title = WizardKeys.getTitle(helpkey);
		return (title == null) ? "" : title;
	}

}
 