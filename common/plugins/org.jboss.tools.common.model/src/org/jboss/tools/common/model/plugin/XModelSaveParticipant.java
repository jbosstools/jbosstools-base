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
package org.jboss.tools.common.model.plugin;

import java.util.ArrayList;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;

public class XModelSaveParticipant implements ISaveParticipant {
	ArrayList<XModel> models = new ArrayList<XModel>();
	ArrayList<XModel> toSave = new ArrayList<XModel>();
	
	public void addModel(XModel model) {
		 if(!models.contains(model)) models.add(model);
	}
	
	public void removeModel(XModel model) {
		int i = models.indexOf(model);
		if(i >= 0) models.remove(i);
	}

	public void prepareToSave(ISaveContext context) throws CoreException {
	}
	
	private void check() {
		toSave.clear();
		XModelObject[] modified = getModified();
		if(modified.length == 0) return;
/*
		String mes = "Struts project ";
		for (int i = 0; i < modified.length; i++) {
			if(i > 0) mes += ", ";
			mes += modified[i].getPresentationString();
		}
		mes += " is modified.\n Do you want to save changes?";
		try {		  
			MessageDialog d = new MessageDialog(null, "Save", null, mes, MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
			int q = d.open();
			if(q != 0) return;
		} catch (Exception e) {
			XStudioPlugin.log("Warning: could not call save dialog from XStudioSaveParticipant.", e);
		}
*/
		for (int i = 0; i < modified.length; i++) toSave.add(modified[i].getModel());
	}

	public void saving(ISaveContext context) throws CoreException {
		check();
		XModel[] ms = (XModel[])toSave.toArray(new XModel[0]);
		for (int i = 0; i < ms.length; i++) ms[i].save();		
	}

	public void rollback(ISaveContext context) {}

	public void doneSaving(ISaveContext context) {
		toSave.clear();
	}
	
	private XModelObject[] getModified() {
		XModel[] ms = models.toArray(new XModel[0]);
		ArrayList<XModelObject> l = new ArrayList<XModelObject>();
		for (int i = 0; i < ms.length; i++) {
			IProject p = (IProject)ms[i].getProperties().get("project");
			if(p == null || !p.isOpen()) continue;
			if(ms[i].getRoot().isModified()) {
				XModelObject fs = FileSystemsHelper.getFileSystems(ms[i]);
				if(fs != null) l.add(fs);
			}
		}
		return l.toArray(new XModelObject[0]);
	}

}
