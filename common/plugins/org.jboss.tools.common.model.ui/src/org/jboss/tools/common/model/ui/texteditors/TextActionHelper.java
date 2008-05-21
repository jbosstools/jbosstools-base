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

import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.jboss.tools.common.model.ui.texteditors.propertyeditor.PropertyEditorMessages;

public class TextActionHelper {
	
	public static void addCutAction(ITextEditor editor) {
		String property = "Editor.Cut.";
		int operationTarget = ITextOperationTarget.CUT;
		String contextId = IAbstractTextEditorHelpContextIds.CUT_ACTION;
		String definitionId = ITextEditorActionDefinitionIds.CUT;
		String actionId = ITextEditorActionConstants.CUT;
		addAction(editor, property, operationTarget, contextId, definitionId, actionId);
	}
	
	public static void addCopyAction(ITextEditor editor) {
		String property = "Editor.Copy.";
		int operationTarget = ITextOperationTarget.COPY;
		String contextId = IAbstractTextEditorHelpContextIds.COPY_ACTION;
		String definitionId = ITextEditorActionDefinitionIds.COPY;
		String actionId = ITextEditorActionConstants.COPY;
		addAction(editor, property, operationTarget, contextId, definitionId, actionId);
	}
	
	public static void addPasteAction(ITextEditor editor) {
		String property = "Editor.Paste.";
		int operationTarget = ITextOperationTarget.PASTE;
		String contextId = IAbstractTextEditorHelpContextIds.PASTE_ACTION;
		String definitionId = ITextEditorActionDefinitionIds.PASTE;
		String actionId = ITextEditorActionConstants.PASTE;
		addAction(editor, property, operationTarget, contextId, definitionId, actionId);
	}
	
	public static void addDeleteAction(ITextEditor editor) {
		String property = "Editor.Delete.";
		int operationTarget = ITextOperationTarget.DELETE;
		String contextId = IAbstractTextEditorHelpContextIds.DELETE_ACTION;
		String definitionId = ITextEditorActionDefinitionIds.DELETE;
		String actionId = ITextEditorActionConstants.DELETE;
		addAction(editor, property, operationTarget, contextId, definitionId, actionId);
	}
	
	public static void addAction(ITextEditor editor, String property,
			int operationTarget, String contextId, String definitionId, String actionId) {
		TextOperationAction action = new TextOperationAction(PropertyEditorMessages.getResourceBundle(), 
				property, editor, operationTarget, true);
		action.setHelpContextId(contextId);
		action.setActionDefinitionId(definitionId);
		editor.setAction(actionId, action);
	}

}
