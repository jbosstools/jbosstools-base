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
package org.jboss.tools.common.model.ui.editor;

import java.util.*;
import org.eclipse.ui.*;

public class EditorActionBarContributorWrapper implements IEditorActionBarContributor {
	IActionBars bars;
	IWorkbenchPage page;
	Map<String,IEditorActionBarContributor> contributors = new HashMap<String,IEditorActionBarContributor>();
	IEditorActionBarContributor active;
	
	public void init(IActionBars bars, IWorkbenchPage page) {
		this.bars = bars;
		this.page = page;
	}

	public void setActiveEditor(IEditorPart targetEditor) {
		IEditorPart p = (targetEditor instanceof EditorPartWrapper) ? ((EditorPartWrapper)targetEditor).getEditor() : targetEditor;
		String entity = (targetEditor instanceof EditorPartWrapper) ? ((EditorPartWrapper)targetEditor).getEntity() : null;
		IEditorActionBarContributor c = getContributor(entity);
		if(c != active && c!=null) {
			if(active != null) {
				active.setActiveEditor(null);
				//active.dispose();
			}
			active = c;
			bars.getMenuManager().removeAll();
			bars.getToolBarManager().removeAll();
			bars.getStatusLineManager().removeAll();
			active.init(bars, page);
			bars.updateActionBars();
			bars.getToolBarManager().update(false);
		}
		if (active!=null) active.setActiveEditor(p);
	}

	public void dispose() {
		if(active != null) active.dispose();
	}
	
	public IEditorActionBarContributor getActiveContributer() {
		return active;
	}
	
	IEditorActionBarContributor getContributor(String entity) {
		if(entity == null) entity = "";
		IEditorActionBarContributor c = (IEditorActionBarContributor)contributors.get(entity);
		if(c == null) {
			if(entity != null) {
				EditorPartFactory f = EditorPartWrapperExtension.getInstance().getFactory(entity);
				if(f != null) c = f.createEditorActionBarContributor();
			}
///			if(c == null) c = new XMLActionContributor();
			if(c != null) contributors.put(entity, c);
		}
		return c;
	}

}
