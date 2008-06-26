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

import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class SaveEditorsSpecialWizard implements SpecialWizard {
	public void setObject(Object object) {}
	
	public int execute() {
		IWorkbenchPage workbenchPage = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();		
		IEditorPart[] es = workbenchPage.getDirtyEditors();
		if(es != null) for (int i = 0; i < es.length; i++) es[i].doSave(null);
		return 0;	
	}

}
