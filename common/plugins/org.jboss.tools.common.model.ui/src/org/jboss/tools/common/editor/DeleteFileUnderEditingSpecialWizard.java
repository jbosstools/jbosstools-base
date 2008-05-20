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

import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class DeleteFileUnderEditingSpecialWizard implements SpecialWizard {
	XModelObject o;

	public void setObject(Object object) {
		o = (XModelObject)object;
	}

	public int execute() {
		IWorkbenchPage workbenchPage = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
		String path = o.getPath();
		if(path == null) return 1;
		IEditorReference[] es = workbenchPage.getEditorReferences();
		if(es != null) for (int i = 0; i < es.length; i++) {
			IEditorPart editor = es[i].getEditor(false);
			if(editor == null) continue;
			XModelObject eo = (XModelObject)editor.getEditorInput().getAdapter(XModelObject.class);
			if(eo == null) continue;
			String eop = eo.getPath();
			if(eop != null && (eop + "/").startsWith(path + "/")) {
				if(!editor.isDirty()) {
					workbenchPage.closeEditor(editor, false);
				} else {
					//Improve: do not close and let object be restored
					workbenchPage.closeEditor(editor, false);
				}
			}
		}
		return 0;
	}

}
