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

import java.text.MessageFormat;
import java.util.Properties;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.Preference;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.common.core.resources.*;

public class OpenEditorWizard implements SpecialWizard {
	Properties p;
	
	public void setObject(Object object) {
		p = (Properties)object;
	}
	
	public int execute() {
		try {
			IEditorPart editor = null;
			XModelObject object = (XModelObject)p.get("object"); //$NON-NLS-1$
			IWorkbenchPage workbenchPage = getWorkbenchPage();
			if(workbenchPage == null) return 0;
			String id = object.getModelEntity().getEditorClassName();
			if("FileXML".equals(object.getModelEntity().getName())) {
				id = null;
			}
			if(id != null && (id.length() == 0 || id.equals("DefaultEditor"))) { //$NON-NLS-1$
				id = null;
			} else {
				boolean b = "yes".equals(PreferenceModelUtilities.getPreferenceModel().getByPath(Preference.EDITOR_PATH).getAttributeValue("useRedHatEditors")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				if(!b) id = null;
			}
			IModelObjectEditorInput input = XModelObjectEditorInput.createInstance(object);
			if(input == null) throw new IllegalStateException("Cannot find resource for object " + object.getPresentationString()); //$NON-NLS-1$
			if(input instanceof IFileEditorInput) {
				IFileEditorInput fei = (IFileEditorInput)input;
				IFile f = fei.getFile();
				if(f != null && !f.isSynchronized(IResource.DEPTH_INFINITE)) {
					try {
						f.getParent().refreshLocal(IResource.DEPTH_INFINITE, null);
					} catch (CoreException e) {
						ModelUIPlugin.getPluginLog().logError(e);
					}
				}
				if(f == null || !f.exists()) {
					ServiceDialog d = object.getModel().getService();
					Object pathData = f == null ? object.getPath() : f.getFullPath();
					String message = MessageFormat.format(
							"The file {0} was removed externally.", pathData);
					d.showDialog("Warning", message, new String[]{"Close"}, null, ServiceDialog.WARNING);
					object.getModel().update();
					return 1;
				}
			}
			if("true".equals(p.getProperty("onlySelectIfOpen"))) { //$NON-NLS-1$ //$NON-NLS-2$
				editor = workbenchPage.findEditor(input);
				if(editor != null) workbenchPage.bringToTop(editor);
			} else if(id == null) {
				if(input instanceof IFileEditorInput) {
					IFile f = ((IFileEditorInput)input).getFile();
					IDE.setDefaultEditor(f, null);
					IEditorDescriptor d = IDE.getEditorDescriptor(f);
					if(d != null) id = d.getId();
					if(id != null) {
						editor = IDE.openEditor(workbenchPage, f, id, true);
					} else {
						editor = IDE.openEditor(workbenchPage, f, true);
					}
				} else {
					if(id == null) {
						IEditorDescriptor d = IDE.getEditorDescriptor(input.getName());
						if(d != null) id = d.getId();
						if(id == null) id = "org.eclipse.ui.DefaultTextEditor";
					}
					editor = workbenchPage.openEditor(input, id);
				}
			} else {
				editor = workbenchPage.openEditor(input, id);
			}
			if(p != null && editor != null) p.put("editor", editor); //$NON-NLS-1$
			if(p != null && "true".equals(p.get("toErrorTab")) && (editor instanceof ObjectMultiPageEditor)) { //$NON-NLS-1$ //$NON-NLS-2$
				ObjectMultiPageEditor m = (ObjectMultiPageEditor)editor;
				m.activateErrorTab();
			}
		} catch (XModelException e) {
			p.put("exception", e); //$NON-NLS-1$
			return 1;
		} catch (PartInitException e) {
			p.put("exception", e); //$NON-NLS-1$
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
