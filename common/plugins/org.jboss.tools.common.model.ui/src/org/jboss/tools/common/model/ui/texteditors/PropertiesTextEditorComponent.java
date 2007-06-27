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

import java.util.ResourceBundle;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.common.editor.ObjectTextEditor;
import org.jboss.tools.common.propertieseditor.text.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.texteditors.propertyeditor.PropertiesTextEditorStub;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.ITextListener;
import org.eclipse.jface.text.TextEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProviderExtension;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.ResourceAction;
import org.eclipse.ui.texteditor.RevertToSavedAction;
import org.eclipse.ui.texteditor.SaveAction;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;

/**
 * @author Jeremy
 *
 */
public class PropertiesTextEditorComponent extends PropertiesTextEditorStub implements ITextListener, ITextProvider, ObjectTextEditor {
	protected TextEditorSupport support = createSupport();
	protected boolean isObjectNull = false;
	
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		dnd.enable();
		setModified(false);
		getSourceViewer().getDocument().addDocumentListener(this);
	}

	public PropertiesTextEditorComponent() {
		support.setProvider(this);
	}

	protected TextEditorSupport createSupport() {
		return new PropertyTextEditorSupport();
	}
	
	public TextEditorSupport getSupport() {
		return support; 
	}
	
	public void setObject(XModelObject object) {
		isObjectNull = (object == null);
		if(isObjectNull) return;
		getSourceViewer().getDocument().removeDocumentListener(this);
		support.setObject(object);
		getSourceViewer().getDocument().addDocumentListener(this);
	}
	
	public void updateDocument() {
		support.update();
	}
	
	public void dispose() {
		super.dispose();
	}

	public boolean isDirty() {
		return (isObjectNull) ? super.isDirty() : false;
	}
	
	public String getText() {
		String text = null;
		try {
			text = getSourceViewer().getDocument().get();
		} catch (Exception ex) {}
		return (text == null) ? "" : text;
	}

	public boolean isEditable() {
		return !isEditorInputReadOnly();
	}

	public boolean isEditorInputReadOnly() {
		if(!(getEditorInput() instanceof IModelObjectEditorInput)) return super.isEditorInputReadOnly();
		IModelObjectEditorInput input = (IModelObjectEditorInput)getEditorInput();
		return input != null && input.getXModelObject() != null && !input.getXModelObject().isObjectEditable();
	}

	public void setModified(boolean set) {
		if(set == support.isModified()) return;
		support.setModified(set);		
		firePropertyChange(IEditorPart.PROP_DIRTY);
	}
	
	public boolean isModified() {
		return support.isModified();
	}

	protected void doSetInput(IEditorInput input) throws CoreException {
		super.doSetInput(input);
		if(getSourceViewer() != null && getSourceViewer().getDocument() != null) {
			getSourceViewer().getDocument().removeDocumentListener(this);
			getSourceViewer().getDocument().addDocumentListener(this);
		}
		
	}
	public void save() {
		if(!isObjectNull && !support.canSave(false)) return;
		if(isObjectNull) super.doSave(null); else support.save();
	}

	public void addFocusListener(FocusListener listener) {
		getSourceViewer().getTextWidget().addFocusListener(listener);
	}
	
	public void removeFocusListener(FocusListener listener) {
		getSourceViewer().getTextWidget().removeFocusListener(listener);
	}
	
	public void setCursor(int line, int position) {
		try {
			int i = getSourceViewer().getDocument().getLineOffset(line - 1) + position -1;
			getSourceViewer().setSelectedRange(i, 0);
		} catch (Exception e) {}
	}

	public void doSaveAs() {}
	
	public void doSave(IProgressMonitor monitor){
		if(isObjectNull) super.doSave(monitor);
	}
	
	private boolean textChangedLock = false;

	public void textChanged(TextEvent event) {
		if(textChangedLock) {
			firePropertyChange(IEditorPart.PROP_DIRTY);
		} else {
			setModified(true);
		}
	}

	public void setText(String text) {
		textChangedLock = true;
		try {
			if(getSourceViewer() == null || getSourceViewer().getDocument() == null) return;
			String txt = getSourceViewer().getDocument().get();
			if(txt != null && txt.length() > 0) {
				if(!TextMerge.replace(getSourceViewer().getDocument(), text)) {
					getSourceViewer().getDocument().set(text);
				}
			} else {
				getSourceViewer().getDocument().set(text);
			}
		} finally {
			textChangedLock = false;
		}
	}

	public boolean isEqualText(String text) {
		return (getSourceViewer().getDocument() != null && text.equals(getText()));
	}
	
	public void doSanityCheckState(IEditorInput input) {
		super.safelySanityCheckState(input);
		if(isObjectNull) return;
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					try { Thread.sleep(200); } catch (Exception e) {}
					support.save();
				}
			}
		);			
	}

	public void doRevertToSaved() {
		support.save();
		support.revertToSaved();
	}
	
	protected void createActions() {
		super.createActions();
		ResourceAction action = new RevertToSavedAction3(this);
		action.setHelpContextId(IAbstractTextEditorHelpContextIds.REVERT_TO_SAVED_ACTION);
		action.setActionDefinitionId(ITextEditorActionDefinitionIds.REVERT_TO_SAVED);
		setAction(ITextEditorActionConstants.REVERT_TO_SAVED, action);

		action = new SaveAction3(this);
		action.setHelpContextId(IAbstractTextEditorHelpContextIds.SAVE_ACTION);
		action.setActionDefinitionId(ITextEditorActionDefinitionIds.SAVE);
		setAction(ITextEditorActionConstants.SAVE, action);
		markAsPropertyDependentAction(ITextEditorActionConstants.SAVE, true);
	}
	
	public void selectModelObject(XModelObject object) {
		String text = getText();
		String name = object.getAttributeValue("name");
		String nvs = object.getAttributeValue("name-value-separator");
		int i = text.indexOf(name + nvs);
		if(i < 0) return;
		int j = text.indexOf('\n', i);
		if(j < 0) j = text.length();
		selectAndReveal(i, j - i);
	}
	
	public void selectModelObject(XModelObject object, String attribute) {
		selectModelObject(object);
	}

	protected void handleEditorInputChanged() {
		final IDocumentProvider provider= getDocumentProvider();
		if (provider == null) {
			// fix for http://dev.eclipse.org/bugs/show_bug.cgi?id=15066
			close(false);
			return;
		}		
		final IEditorInput input= getEditorInput();
		if (provider.isDeleted(input)) {			
			if (isSaveAsAllowed()) {				
				if (true) {
					IProgressMonitor pm= getProgressMonitor();
					performSaveAs(pm);
					if (pm.isCanceled())
						handleEditorInputChanged();
				} else {
					close(false);
				}
				
			} else {
				if (true) close(false);
			}			
		} else {			
			if (true) {				
				try {
					if (provider instanceof IDocumentProviderExtension) {
						IDocumentProviderExtension extension= (IDocumentProviderExtension) provider;
						extension.synchronize(input);
					} else {
						doSetInput(input);
					} 
				} catch (CoreException x) {
					ModelUIPlugin.getPluginLog().logError(x);
				}
			}
		}
	}

	public void documentAboutToBeChanged(DocumentEvent event) {}

	public void documentChanged(DocumentEvent event) {
		textChanged(null);
	}
	
	public void updateModification() {
		XModelObject object = support.getModelObject();
		if(object != null && !object.isModified() && support.isModified()) {
			//external update
			setModified(false);
		} else {
			firePropertyChange(ITextEditor.PROP_DIRTY);
		}
	}

	public XModelObject findModelObjectAtCursor() {
		return null;
	}
}

class RevertToSavedAction3 extends RevertToSavedAction {
	PropertiesTextEditorComponent t;
	RevertToSavedAction3(PropertiesTextEditorComponent t) {
		super(ResourceBundle.getBundle("org.eclipse.ui.texteditor.ConstructedEditorMessages"), "Editor.Revert.", t);
		this.t = t;
	}
	public void update() {
		setEnabled(t != null && t.support != null && (t.support.canRevertToSaved() || t.isModified()));
	}
}

class SaveAction3 extends SaveAction {
	PropertiesTextEditorComponent t;
	SaveAction3(PropertiesTextEditorComponent t) {
		super(ResourceBundle.getBundle("org.eclipse.ui.texteditor.ConstructedEditorMessages"), "Editor.Save.", t);
		this.t = t;
	}
	public void update() {
		XModelObject o = (t == null) ? null : t.getModelObject();
		setEnabled(o != null && o.isModified());
	}
	
	public void run() {
		IEditorPart p = getTextEditor().getSite().getPage().findEditor(t.getEditorInput());
		if(p != null) getTextEditor().getSite().getPage().saveEditor(p, false);
	}
	
}
