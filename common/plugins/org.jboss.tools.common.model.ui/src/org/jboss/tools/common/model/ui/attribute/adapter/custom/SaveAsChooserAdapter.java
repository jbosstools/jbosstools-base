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
package org.jboss.tools.common.model.ui.attribute.adapter.custom;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.model.ui.*;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.SaveAsDialog;

import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class SaveAsChooserAdapter extends DefaultValueAdapter implements IActionHelper {

	public String getCommand() {
		return "Browse...";
	}

	public String invoke(Control control) {
		Shell shell = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		SaveAsDialog dialog = new SaveAsDialog(shell);
		IFile original = null;
		String v = getStringValue(false);
		if(v != null && v.length() > 0) {
			original = EclipseResourceUtil.getFile(v);
		}
		if (original != null) dialog.setOriginalFile(original);		
		dialog.create();			
		if (dialog.open() == Dialog.CANCEL) return null;
		IPath filePath = dialog.getResult();
		original = ModelUIPlugin.getWorkspace().getRoot().getFile(filePath);
		return original.getLocation().toString().replace('\\', '/');
	}

	public Object getAdapter(Class adapter) {
		if (adapter == IActionHelper.class) return this;
		return super.getAdapter(adapter);
	}

}
