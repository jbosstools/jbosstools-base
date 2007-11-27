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
package org.jboss.tools.common.model.ui.texteditors.propertyeditor;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.editors.text.TextEditorActionContributor;

/**
 * @author Jeremy
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class PropertyActionContributor extends TextEditorActionContributor {
//	protected RetargetTextEditorAction fContentAssistProposal;
//	protected RetargetTextEditorAction fContentAssistTip;

	/**
	 * Default constructor.
	 */
	public PropertyActionContributor() {
		super();
//		fContentAssistProposal= new RetargetTextEditorAction(XMLEditorMessages.getResourceBundle(), "ContentAssistProposal."); //$NON-NLS-1$
//		fContentAssistProposal.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS); 
//		fContentAssistTip= new RetargetTextEditorAction(XMLEditorMessages.getResourceBundle(), "ContentAssistTip."); //$NON-NLS-1$
//		fContentAssistTip.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
	}
	
	/*
	 * @see IEditorActionBarContributor#init(IActionBars)
	 */
	public void init(IActionBars bars) {
		super.init(bars);
		
		IMenuManager menuManager= bars.getMenuManager();
		IMenuManager editMenu= menuManager.findMenuUsingPath(IWorkbenchActionConstants.M_EDIT);
		if (editMenu != null) {
//			editMenu.add(new Separator());
//			editMenu.add(fContentAssistProposal);
//			editMenu.add(fContentAssistTip);
		}	
		
		IToolBarManager toolBarManager= bars.getToolBarManager();
		if (toolBarManager != null) {
//			toolBarManager.add(new Separator());
		}
	}
	
	private void doSetActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);

//		ITextEditor editor= null;
//		if (part instanceof ITextEditor)
//			editor= (ITextEditor) part;

//		IAction a = getAction(editor, "ContentAssistProposal");
//		fContentAssistProposal.setAction(getAction(editor, "ContentAssistProposal")); //$NON-NLS-1$
//		a = getAction(editor, "ContentAssistTip");
//		fContentAssistTip.setAction(getAction(editor, "ContentAssistTip")); //$NON-NLS-1$

	}
	
	/*
	 * @see IEditorActionBarContributor#setActiveEditor(IEditorPart)
	 */
	public void setActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);
		doSetActiveEditor(part);
	}
	
	/*
	 * @see IEditorActionBarContributor#dispose()
	 */
	public void dispose() {
		doSetActiveEditor(null);
		super.dispose();
	}

}
