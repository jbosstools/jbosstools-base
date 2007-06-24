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
package org.jboss.tools.common.editor;

import java.util.Properties;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.eclipse.core.resources.*;
import org.jboss.tools.common.core.resources.*;

public class OpenEditorWizard implements SpecialWizard {
	Properties p;
	
	public void setObject(Object object) {
		p = (Properties)object;
	}
	
	public int execute() {
		try {
			IEditorPart editor = null;
			XModelObject object = (XModelObject)p.get("object");
			IWorkbenchPage workbenchPage = getWorkbenchPage();
			if(workbenchPage == null) return 0;
			String id = object.getModelEntity().getEditorClassName();
			if(id != null && (id.length() == 0 || id.equals("DefaultEditor"))) {
				id = null;
			} else {
				boolean b = "yes".equals(PreferenceModelUtilities.getPreferenceModel().getByPath("%Options%/Struts Studio/Editors").getAttributeValue("useRedHatEditors"));
				if(!b) id = null;
			}
			IModelObjectEditorInput input = XModelObjectEditorInput.createInstance(object);
			if(input == null) throw new Exception("Cannot find resource for object " + object.getPresentationString());
			if(input instanceof IFileEditorInput) {
				IFileEditorInput fei = (IFileEditorInput)input;
				IFile f = fei.getFile();
				if(!f.isSynchronized(IResource.DEPTH_INFINITE)) {
					try {
						f.getParent().refreshLocal(IResource.DEPTH_INFINITE, null);
					} catch (Exception e) {
						ModelUIPlugin.log(e);
					}
				}
				if(!f.exists()) {
					ServiceDialog d = object.getModel().getService();
					String message = "The file " + f.getLocation().toString() + " was removed externally.";
					d.showDialog("Warning", message, new String[]{"Close"}, null, ServiceDialog.WARNING);
					object.getModel().update();
					return 1;
				}
			}
			if("true".equals(p.getProperty("onlySelectIfOpen"))) {
				editor = workbenchPage.findEditor(input);
				if(editor != null) workbenchPage.bringToTop(editor);
			} else if(id == null) {
				if(input instanceof IFileEditorInput) {
					editor = IDE.openEditor(workbenchPage, ((IFileEditorInput)input).getFile(), true);
				} else {
					editor = workbenchPage.openEditor(input, id);
				}
			} else {
				editor = workbenchPage.openEditor(input, id);
			}
			if(p != null && editor != null) p.put("editor", editor);
			if(p != null && "true".equals(p.get("toErrorTab")) && (editor instanceof ObjectMultiPageEditor)) {
				ObjectMultiPageEditor m = (ObjectMultiPageEditor)editor;
				m.activateErrorTab();
			}
		} catch (Exception e) {
			p.put("exception", e);
			return 1;
		}
		return 0;
	}
	
	private IWorkbenchPage getWorkbenchPage() {
		ModelUIPlugin plugin = ModelUIPlugin.getDefault();
		IWorkbench workbench = (plugin == null) ? null : plugin.getWorkbench();
		IWorkbenchWindow window = (workbench == null) ? null : workbench.getActiveWorkbenchWindow();
		return (window == null) ? null : window.getActivePage();
	}
	
}
