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
package org.jboss.tools.common.model.ui.texteditors;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.text.xml.xpl.ToggleOccurencesMarkUpAction;

/**
 * @author Jeremy
 *
 */
public class MultiPageContributor extends AbstractMultiPageContributor {
	protected FakeTextEditor fakeTextEditor = new FakeTextEditor();

	public MultiPageContributor() {
		fToggleOccurencesMarkUp = new ToggleOccurencesMarkUpAction();
	}
	
	public void init(IActionBars bars) {
		super.init(bars);
		initEditMenu(bars);
	}

	public void setActivePage(IEditorPart part) {
//		if (fActiveEditorPart == part) return;
		cleanStatusLine();
		fActiveEditorPart = part;	
		IActionBars actionBars = getActionBars();		
		if (actionBars != null) {
	
			ITextEditor editor = (part instanceof ITextEditor) ? (ITextEditor) part : null;

			if (editor!=null) {

				actionBars.setGlobalActionHandler(ActionFactory.DELETE.getId(), getAction(editor, ITextEditorActionConstants.DELETE));
				actionBars.setGlobalActionHandler(ActionFactory.UNDO.getId(), getAction(editor, ITextEditorActionConstants.UNDO));
				actionBars.setGlobalActionHandler(ActionFactory.REDO.getId(), getAction(editor, ITextEditorActionConstants.REDO));
				actionBars.setGlobalActionHandler(ActionFactory.CUT.getId(), getAction(editor, ITextEditorActionConstants.CUT));
				actionBars.setGlobalActionHandler(ActionFactory.COPY.getId(), getAction(editor, ITextEditorActionConstants.COPY));
				actionBars.setGlobalActionHandler(ActionFactory.PASTE.getId(), getAction(editor, ITextEditorActionConstants.PASTE));
				actionBars.setGlobalActionHandler(ActionFactory.SELECT_ALL.getId(), getAction(editor, ITextEditorActionConstants.SELECT_ALL));
				actionBars.setGlobalActionHandler(ActionFactory.FIND.getId(), getAction(editor, ITextEditorActionConstants.FIND));
				actionBars.setGlobalActionHandler(IDEActionFactory.BOOKMARK.getId(), getAction(editor, IDEActionFactory.BOOKMARK.getId()));
				actionBars.setGlobalActionHandler(IDEActionFactory.ADD_TASK.getId(), getAction(editor, IDEActionFactory.ADD_TASK.getId()));
				actionBars.setGlobalActionHandler(ActionFactory.PRINT.getId(), getAction(editor, ITextEditorActionConstants.PRINT));
				actionBars.setGlobalActionHandler(ActionFactory.REVERT.getId(), getAction(editor, ITextEditorActionConstants.REVERT));
				actionBars.setGlobalActionHandler(ActionFactory.SAVE.getId(), getAction(editor, ITextEditorActionConstants.SAVE));

			}
			// re-register action on key binding service
			IEditorPart localPart = (part!=null)?part:mainPart;
			IHandlerService handler = (IHandlerService)localPart.getEditorSite().getService(IHandlerService.class);
			if (editor!=null) {
				// editor
				registerKeyBindings(handler, ACTIONS_2, editor);
			} else {
				//fakeTextEditor
				registerKeyBindings(handler, ACTIONS_1, fakeTextEditor);
			}

			cleanActionBarStatus();
			actionBars.updateActionBars();
		}
		
		try {
			fToggleOccurencesMarkUp.setEditor(getTextEditor(part));
			fToggleOccurencesMarkUp.update();
		} catch (Exception x) {
			ModelUIPlugin.log(x);
		}

		updateStatus();
	}
	
	class FakeTextEditor extends AFakeTextEditor {
	}
}
