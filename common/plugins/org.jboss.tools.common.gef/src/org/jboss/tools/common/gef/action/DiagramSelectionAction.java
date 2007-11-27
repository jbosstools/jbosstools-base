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
package org.jboss.tools.common.gef.action;

import java.util.List;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPart;


public abstract class DiagramSelectionAction extends SelectionAction {

	public DiagramSelectionAction(IWorkbenchPart editor) {
		super(editor);
	}

	protected abstract Command createCommand(List objects);


	/**
	 * Returns <code>true</code> if the selected objects can
	 * be deleted.  Returns <code>false</code> if there are
	 * no objects selected or the selected objects are not
	 * {@link EditPart}s.
	 */
	protected boolean calculateEnabled() {
		Command cmd = createCommand(getSelectedObjects());
		if (cmd == null)
			return false;
		return cmd.canExecute();
	}

	/**
	 * Performs the delete action on the selected objects.
	 */
	public void run() {
		execute(createCommand(getSelectedObjects()));
	}

	public void update() {
		IEditorPart editor = (IEditorPart)getWorkbenchPart();
		if(editor == null) return;
		IDiagramSelectionProvider provider = (IDiagramSelectionProvider)editor.getAdapter(IDiagramSelectionProvider.class);
		if(provider == null) return;
		setSelection(provider.getSelection());
	}

}
