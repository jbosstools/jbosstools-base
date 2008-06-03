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

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;

public class PaletteDialogState {
	PaletteDialog dialog;
	
	public PaletteDialogState(PaletteDialog dialog) {
		this.dialog = dialog;
	}

	Rectangle bounds = null;
	int[] ws = null; 
	Object[] es = new Object[0];
	Object selected = null;
	int v_scroll = -1;
	
	public void saveState() {
		bounds = dialog.getShell().getBounds();
		es = dialog.treeViewer.getExpandedElements();
		saveSelection();
		v_scroll = dialog.treeViewer.getTree().getVerticalBar().getSelection();
		ws = dialog.sash.getWeights();
	}
	
	public void loadState() {
		if(bounds != null) {
			dialog.getShell().setBounds(bounds.x, bounds.y, bounds.width, bounds.height);
		} else {
			Rectangle r = Display.getDefault().getClientArea();
			int w = 620, h = 420, x = (r.width - w) / 2, y = (r.height - h) / 2;
			dialog.getShell().setBounds(x, y, w, h);
		}  
		loadSash();
		loadExpansion();
		loadSelection();
		if(v_scroll > 0) dialog.treeViewer.getTree().getVerticalBar().setSelection(v_scroll);
	}
	
	void loadSash() {
		if(ws == null || ws.length == 0) ws = new int[]{25, 50};
		dialog.sash.setWeights(ws); 
	}
	
	void loadExpansion() {
		if(es == null || es.length < 2) {
			XModel model = PreferenceModelUtilities.getPreferenceModel();
			es = new Object[2];
			es[0] = model.getByPath("XStudio");
			es[1] = model.getByPath("XStudio/Palette"); 
		}
		dialog.treeViewer.setExpandedElements(es);
	}
	
	void saveSelection() {
		selected = null;
		ISelection s = dialog.treeViewer.getSelection();
		if(s.isEmpty()) return;
		StructuredSelection ss = (StructuredSelection)s;
		selected = ss.getFirstElement();		
	}
	
	void loadSelection() {
		if(selected == null) selected = PreferenceModelUtilities.getPreferenceModel().getByPath("XStudio/Palette"); 
		dialog.treeViewer.setSelection(new StructuredSelection(selected), true);
		dialog.objectEditor.setModelObject((XModelObject)selected);
	}
	
}
