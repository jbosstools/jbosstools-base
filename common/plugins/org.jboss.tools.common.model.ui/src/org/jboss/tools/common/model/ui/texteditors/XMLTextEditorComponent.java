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

import java.lang.reflect.InvocationTargetException;
import java.util.Properties;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextEvent;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProviderExtension;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.ResourceAction;
import org.eclipse.ui.texteditor.RevertToSavedAction;
import org.eclipse.ui.texteditor.SaveAction;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.jboss.tools.common.editor.ObjectTextEditor;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.DiscardFileHandler;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.filesystems.impl.FolderLoader;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.messages.UIMessages;
import org.jboss.tools.common.model.ui.texteditors.xmleditor.XMLTextEditor;
import org.jboss.tools.common.model.util.PositionSearcher;

public class XMLTextEditorComponent extends XMLTextEditor implements ObjectTextEditor, ITextProvider {
	protected TextEditorSupport support = createSupport();
	protected boolean isObjectNull = false;
	private ICL changeListener = null;
	protected boolean isStandAlone = false;
	
	public XMLTextEditorComponent() {
		this(true);
	}

	public XMLTextEditorComponent(boolean useUseRHDSConfig) {
		super(useUseRHDSConfig);
		support.setProvider(this);
		///		XmlDocumentProvider p = (XmlDocumentProvider)getDocumentProvider();
		///		p.disableElementContentChange();
		ModelPlugin.getWorkspace().addResourceChangeListener(changeListener = new ICL());
	}

	protected TextEditorSupport createSupport() {
		return new TextEditorSupport();
	}
	
	public TextEditorSupport getSupport() {
		return support; 
	}

	boolean isNullEditor = false;

	public void createPartControl(Composite parent) {
		if(getModel() == null) {
			setObject(null);
			isNullEditor = true;
			Label label = new Label(parent, SWT.NONE);
			label.setText(UIMessages.CANNOT_DISPLAY_DOCUMENT_CONTENTS);
			label.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
			return;
		} else {
			super.createPartControl(parent);
		}
	}

	public void setObject(XModelObject object) {
		if(isNullEditor) return;
		isObjectNull = (object == null);
		if(isObjectNull) return;
		getDocumentListenerRegister().unregister();
		try {
			support.setObject(object);
		} finally {
			getDocumentListenerRegister().register();
		}
	}
	
	public void updateDocument() {
		support.update();
	}
	
	public boolean isDirty() {
		return (isObjectNull || isStandAlone) ? super.isDirty() : false;
	}
	
	public String getText() {
		String text = null;
		ISourceViewer sv = getSourceViewer();
		if(sv == null) return "";
		IDocument d = sv.getDocument();
		if(d == null) return "";
		text = d.get();
		return (text == null) ? "" : text;
	}

	public boolean isEditable() {
		return !isEditorInputReadOnly();
	}

	public boolean isEditorInputReadOnly() {
		if(!(getEditorInput() instanceof IModelObjectEditorInput)) return super.isEditorInputReadOnly();
		IModelObjectEditorInput input = (IModelObjectEditorInput)getEditorInput();
		if(input == null || input.getXModelObject() == null) return true;
		XModelObject o = input.getXModelObject();
		return !o.isObjectEditable() && !o.isAttributeEditable("body");
	}

	public void setModified(boolean set) {
		if(set == support.isModified()) return;
		support.setModified(set);		
		firePropertyChange(IEditorPart.PROP_DIRTY);
	}
	
	public boolean isModified() {
		return support.isModified();
	}

	public void save() {
		if(isStandAlone) {
			super.save();
		} else {
			if(!isObjectNull && !support.canSave(false)) return;
			if(isObjectNull) super.doSave(null); else support.save();
		}
	}

	protected void doSaveYourself() {
		XModelObject o = getModelObject();
		XModelObject f = o == null ? null : o.getParent();
		if(f instanceof FolderImpl) {
			try {
				((FolderImpl)f).saveChild(o);
			} catch (XModelException e) {
				ModelPlugin.getPluginLog().logError(e);
			}
		} else {
			//
		}					
	}
	
	public void addFocusListener(FocusListener listener) {
		ISourceViewer sv = getSourceViewer();
		if (sv != null) sv.getTextWidget().addFocusListener(listener);
	}
	
	public void removeFocusListener(FocusListener listener) {
		ISourceViewer sv = getSourceViewer();
		if (sv != null) getSourceViewer().getTextWidget().removeFocusListener(listener);
	}
	
	public void setCursor(int line, int position) {
		ISourceViewer sv = getSourceViewer();
		if(sv == null || sv.getDocument() == null) return;
		try {
			int i = sv.getDocument().getLineOffset(line - 1) + position -1;
			sv.setSelectedRange(i, 0);
			sv.revealRange(i, 0);
		} catch (BadLocationException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (SWTException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		
	}

	public void doSaveAs() {
		if(isStandAlone) {
			super.doSaveAs();
		}
	}
	
	public void performSaveAs(IProgressMonitor monitor){
		XModelObject old = getModelObject();
		super.performSaveAs(monitor);
		XModelObject o = getModelObject();
		if(o != null) {
			if(o.getParent() instanceof FolderLoader) {
				((FolderLoader)o.getParent()).update();
				if(!o.isActive()) {
					o = getModelObject();
				}
			}
			if(o != null) support.setObject(o);
		}
		if(old.isModified()) try {
			new DiscardFileHandler().executeHandler(old, new Properties());
		} catch (XModelException e) {
			//ignore
//			ModelUIPlugin.log(e);
		}
	}

	public void doSave(IProgressMonitor monitor){
		if(isObjectNull || isStandAlone) super.doSave(monitor);
	}
	
	private int textChangedLock = 0;

	public void textChanged(TextEvent event) {
		if(textChangedLock > 0) {
			firePropertyChange(IEditorPart.PROP_DIRTY);
		} else {
			support.lock++;
			try {
				setModified(true);
			} finally {
				support.lock--;
			}
		}
	}

	public void setText(String text) {
		textChangedLock++;
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
			textChangedLock--;
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
					try {
						Thread.sleep(200);
					} catch (InterruptedException e) {
						//ignore
					}
					support.save(true);
				}
			}
		);			
	}
	
	/**
	 * Handles an external change of the editor's input element.
	 */
	protected void handleEditorInputChanged() {
		final IDocumentProvider provider= getDocumentProvider();
		if (provider == null) {
			// fix for http://dev.eclipse.org/bugs/show_bug.cgi?id=15066
			close(false);
			return;
		}
			
		final IEditorInput input= getEditorInput();
					
		if (provider instanceof IDocumentProviderExtension) {
			WorkspaceModifyOperation operation= new WorkspaceModifyOperation() {
				protected void execute(final IProgressMonitor monitor) throws CoreException {
					IDocumentProviderExtension extension= (IDocumentProviderExtension) provider;
					textChangedLock++;
					try {
						extension.synchronize(input);
					} finally {
						textChangedLock--;
					}
				}
			};
				
			try {
				operation.run(getProgressMonitor());
			} catch (InterruptedException x) {
				//ignore
			} catch (InvocationTargetException x) {
				//ignore
			} 
			
		} else {
				
			try {
				doSetInput(input);
			} catch (CoreException x) {
				//ignore
			}
		}
	}
	
	public void doRevertToSaved() {
		support.save();
		support.revertToSaved();
	}
	
	protected void createActions() {
		super.createActions();
		ResourceAction action = new RevertToSavedAction2(this);
		action.setHelpContextId(IAbstractTextEditorHelpContextIds.REVERT_TO_SAVED_ACTION);
		action.setActionDefinitionId(ITextEditorActionDefinitionIds.REVERT_TO_SAVED);
		setAction(ITextEditorActionConstants.REVERT_TO_SAVED, action);

		action = new SaveAction2(this);
		action.setHelpContextId(IAbstractTextEditorHelpContextIds.SAVE_ACTION);
		action.setActionDefinitionId(ITextEditorActionDefinitionIds.SAVE);
		setAction(ITextEditorActionConstants.SAVE, action);
		markAsPropertyDependentAction(ITextEditorActionConstants.SAVE, true);
	}
	
	public void selectModelObject(XModelObject object) {
		selectModelObject(object, null);
	}
	
	public void dispose() {
		super.dispose();
		if(changeListener != null) {
			ModelPlugin.getWorkspace().removeResourceChangeListener(changeListener);
			changeListener = null;
		}
	}

	public void selectModelObject(XModelObject object, String attribute) {
		PositionSearcher searcher = new PositionSearcher();
		searcher.init(getText(), object, attribute);
		searcher.execute();
		int bp = searcher.getStartPosition();
		int ep = searcher.getEndPosition();
		if(ep >= bp && bp >= 0) {
			selectAndReveal(bp, ep - bp);
		}
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

	class ICL implements IResourceChangeListener {
		public void resourceChanged(IResourceChangeEvent event) {
			if(event == null || event.getDelta() == null || isUpdating) return;
			if(!(getEditorInput() instanceof IFileEditorInput)) return;
			IFile f = ((IFileEditorInput)getEditorInput()).getFile();
			if(f == null) return;
			IMarkerDelta[] md = event.findMarkerDeltas(null, true);
			boolean b = false;
			for (int i = 0; i < md.length && !b; i++) {
				b = f.equals(md[i].getResource());
			}
			if(b) updateAnnotationModel();
		}
	}
	
	boolean isUpdating = false;
	private void updateAnnotationModel() {
	}

	public void gotoMarker(IMarker marker) {
		super.gotoMarker(marker);		
	}
	
	XMLModelObjectFinder modelObjectFinder = new XMLModelObjectFinder();

	public XModelObject findModelObjectAtCursor() {
		XModelObject o = getModelObject();
		if(o == null) return null;
		ISelection selection = getSelectionProvider().getSelection();
		if(!(selection instanceof ITextSelection)) return null;
		int offset = ((ITextSelection)selection).getOffset();
		if(offset < 0) return o;
        IndexedRegion region = getModel().getIndexedRegion(offset);
        return modelObjectFinder.findModelObject(region, o);
	}
	
//	Point storedSelection = new Point(0,0);
//	
//	protected void handleCursorPositionChanged() {
//		super.handleCursorPositionChanged();
//		ISelection selection = getSelectionProvider().getSelection();
//		Point p = getTextViewer().getTextWidget().getSelection();
//		if (storedSelection == null || !storedSelection.equals(p)) {
//			storedSelection = p;
//			if(selection instanceof ITextSelection) {
//				ITextSelection ts = (ITextSelection)selection;
//				if(ts.getLength() == 0) {
//					getSelectionProvider().setSelection(getSelectionProvider().getSelection());
//				}
//			}
//		}
//	}

}

class RevertToSavedAction2 extends RevertToSavedAction {
	XMLTextEditorComponent t;
	RevertToSavedAction2(XMLTextEditorComponent t) {
		super(ResourceBundle.getBundle("org.eclipse.ui.texteditor.ConstructedEditorMessages"), "Editor.Revert.", t);
		this.t = t;
	}
	public void update() {
		setEnabled(t != null && t.support != null && (t.support.canRevertToSaved() || t.isModified()));
	}
}

class SaveAction2 extends SaveAction {
	XMLTextEditorComponent t;
	SaveAction2(XMLTextEditorComponent t) {
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
