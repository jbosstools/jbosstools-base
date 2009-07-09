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

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;

import org.jboss.tools.common.core.resources.XModelObjectEditorInput;

public class SaveEditorSpecialWizard implements SpecialWizard {
	XModelObject o;
	
	public void setObject(Object object) {
		o = (XModelObject)object;
	}
	
	public int execute() {
		if(Display.getCurrent() == null) {
			return 1;
		}
		IWorkbenchWindow window = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
		if(window == null) {			
			return 1;
		}
		IWorkbenchPage workbenchPage = window.getActivePage();
		IModelObjectEditorInput input = XModelObjectEditorInput.createInstance(o);
		if(input == null) return 1;
		IEditorPart editor = workbenchPage.findEditor(input);
		if(editor != null) {
			editor.doSave(null);
			return 0;
		} else {
			return 1;
		}
	}

}
