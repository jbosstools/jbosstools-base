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
package org.jboss.tools.common.model.ui.views.palette.editor;

import org.jboss.tools.common.model.ui.wizards.one.ServiceDialogImpl;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.undo.XUndoManager;

public class PaletteEditor {
	private Shell shell;
	private PaletteDialog dialog;
	
	public PaletteEditor() {}    
    
	public void setObject(Shell shell) {
		this.shell = shell;
	}
    
	public int execute() {
		if(dialog == null) dialog = new PaletteDialog(shell);
		XModel model = ModelUtilities.getPreferenceModel();
		if(model.getService() == null) {
			model.setService(new ServiceDialogImpl());		
		}
		fireTransactionEvent("transaction_begin");
		try {
			XUndoManager undo = model.getUndoManager();
			undo.beginTransaction();
			int code = dialog.open();
			if(code == Window.OK) {
				undo.commitTransaction();
				model.saveOptions();
			} else {
				undo.rollbackTransaction();
			}
			return code;
		} finally {
			if (dialog!=null) dialog.dispose();
			dialog = null;
			fireTransactionEvent("transaction_end");
		}
	}
    
	private void fireTransactionEvent(String kind) {
		XModelImpl m = (XModelImpl)ModelUtilities.getPreferenceModel();
		m.fireStructureChanged(m.getByPath("%Palette%"), 2, kind);
	}
	
}
