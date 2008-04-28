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
package org.jboss.tools.common.model.ui.objecteditor;

import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;

public class SaveUnopenSpecialWizard implements SpecialWizard {
	XModelObject object;

	public void setObject(Object object) {
		this.object = (XModelObject)object;
	}

	public int execute() {
		IWorkbench workbench = ModelUIPlugin.getDefault().getWorkbench();
		IWorkbenchWindow window = (workbench == null) ? null : workbench.getActiveWorkbenchWindow();
		IWorkbenchPage workbenchPage = (window == null) ? null : window.getActivePage();
		if(workbenchPage != null) {
			IModelObjectEditorInput input = XModelObjectEditorInput.createInstance(object);
			if(input == null) return 1;
			IEditorPart editor = workbenchPage.findEditor(input);
			if(editor != null) return 0;
		}
		if(object != null && XActionInvoker.getAction("SaveActions.Save", object) != null) {
			XActionInvoker.invoke("SaveActions.Save", object, null);
		}
		return 0;
	}

}
