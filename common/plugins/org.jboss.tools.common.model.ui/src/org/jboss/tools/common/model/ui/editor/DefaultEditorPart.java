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

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IProgressMonitor;
//import org.jboss.tools.common.core.jdt.Messages;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.texteditor.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.resources.ResourceLayoutManager;
import org.jboss.tools.common.model.ui.texteditors.TextActionHelper;

public class DefaultEditorPart extends EditorPart implements ITextEditor, ITextOperationTarget {
	private Map<String,IAction> actions = new HashMap<String,IAction>();
	private ArrayList<String> actionMapping = new ArrayList<String>();

	public IDocumentProvider getDocumentProvider() {
		return null;
	}

	public void close(boolean save) {
		store();
	}

	public boolean isEditable() {
		return false;
	}

	public void doRevertToSaved() {
	}

	public void setAction(String actionID, IAction action) {
		actions.put(actionID, action);
	}

	public IAction getAction(String id) {
		return actions.get(id);
	}

	public void setActionActivationCode(String actionId, char activationCharacter, int activationKeyCode, int activationStateMask) {
	}

	public void removeActionActivationCode(String actionId) {
	}

	public boolean showsHighlightRangeOnly() {
		return false;
	}

	public void showHighlightRangeOnly(boolean showHighlightRangeOnly) {
	}

	public void setHighlightRange(int offset, int length, boolean moveCursor) {
	}

	public IRegion getHighlightRange() {
		return null;
	}

	public void resetHighlightRange() {}

	public void selectAndReveal(int offset, int length) {}

	public void gotoMarker(IMarker marker) {
	}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		setSite(site);
		setInput(input);
		if (input instanceof IFileEditorInput) {
//			memento = new MementoDOM(ResourceLayoutManager.getDefault().getLayoutElement(getFile(), "TreeFormPage"));
		}
	}

	public void addPropertyListener(IPropertyListener listener) {
	}

	public void createPartControl(Composite parent) {
		createControl(parent);
		createActions();
	}
	
	protected Control createControl(Composite parent) {
		return null;
	}

	public Image getTitleImage() {
		return null;
	}

	public String getTitleToolTip() {
		return null;
	}

	public void removePropertyListener(IPropertyListener listener) {
	}

	public void setFocus() {
	}

	public Object getAdapter(Class adapter) {
		if (ITextOperationTarget.class.equals(adapter))	return this;
		return null;
	}
	
	private IFile getFile() {
		return ((IFileEditorInput)getEditorInput()).getFile();
	}
	
	private void store() {
		IFile file = getFile();
		if(file == null) return;
		ResourceLayoutManager.getDefault().store(file);
	}

	public void doSave(IProgressMonitor monitor) {
		store();
	}

	public void doSaveAs() {
		store();
	}
	
	public boolean isDirty() {
		return false;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public boolean isSaveOnCloseNeeded() {
		return false;
	}

	public boolean canDoOperation(int operation) {
		Collection actions = this.actions.values();
		Iterator i = actions.iterator();
		IAction action;
		while (i.hasNext()) {
			action = (IAction)i.next();
			if(action.isEnabled()) {
				return true;
			}
		}
		return true;
	}

	public void doOperation(int operation) {
		if (operation>actionMapping.size()) {
			ModelUIPlugin.getPluginLog().logError(new RuntimeException("Can not find global action with index: "+operation));
		} else {
			String globalAction = (String)actionMapping.get(operation);
			this.doGlobalAction(globalAction);
		}
	}
	
	protected void createActions() {
		actionMapping.add(null);
		actionMapping.add(ITextOperationTarget.UNDO, ActionFactory.UNDO.getId());
		actionMapping.add(ITextOperationTarget.REDO, ActionFactory.REDO.getId());
		actionMapping.add(ITextOperationTarget.CUT, ActionFactory.CUT.getId());
		actionMapping.add(ITextOperationTarget.COPY, ActionFactory.COPY.getId());
		actionMapping.add(ITextOperationTarget.PASTE, ActionFactory.PASTE.getId());
		actionMapping.add(ITextOperationTarget.DELETE, ActionFactory.DELETE.getId());
		actionMapping.add(ITextOperationTarget.SELECT_ALL, ActionFactory.SELECT_ALL.getId());
		actionMapping.add(ITextOperationTarget.SHIFT_RIGHT, ITextEditorActionConstants.SHIFT_RIGHT);
		actionMapping.add(ITextOperationTarget.SHIFT_LEFT, ITextEditorActionConstants.SHIFT_LEFT);
		actionMapping.add(ITextOperationTarget.PRINT, ActionFactory.PRINT.getId());

		TextActionHelper.addCutAction(this);
		TextActionHelper.addCopyAction(this);
		TextActionHelper.addPasteAction(this);
		TextActionHelper.addDeleteAction(this);

	}

	public void doGlobalAction(String action) {
	}

	public ISelectionProvider getSelectionProvider() {
		return null;
	}

	public void dispose() {
	}

}
