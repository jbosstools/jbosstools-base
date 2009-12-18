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
package org.jboss.tools.common.editor;

import java.io.*;
import java.text.MessageFormat;
import java.util.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.jboss.tools.common.model.util.XModelTreeListenerSWTSync;
import org.jboss.tools.common.model.ui.outline.XModelObjectContentOutlineProvider;
import org.jboss.tools.common.model.ui.select.XModelObjectSelectionProvider;
import org.jboss.tools.common.model.ui.wizards.one.ServiceDialogOption;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.ide.IGotoMarker;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.MultiPageEditorActionBarContributor;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.part.MultiPageEditorSite;
import org.eclipse.ui.texteditor.AbstractTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProviderExtension;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.wst.sse.ui.StructuredTextEditor;
import org.osgi.framework.Bundle;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.options.Preference;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.XModelObjectCache;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.text.ext.IMultiPageEditor;

public class ObjectMultiPageEditor extends MultiPageEditorPart implements XModelTreeListener, IGotoMarker, IMultiPageEditor {
	protected AbstractSectionEditor treeEditor;
	protected TreeFormPage treeFormPage;
	protected ObjectTextEditor textEditor;
	protected IModelObjectEditorInput input;
	protected XModel model = null;
	protected XModelObjectCache cache = null;
	protected XModelObject object = null;
	protected long timeStamp = -1;
	protected long lastModifiedTimeStamp = -1;
	protected boolean isErrorMode = false;
	protected XModelTreeListenerSWTSync syncListener = new XModelTreeListenerSWTSync(this);
	public XModelObjectContentOutlineProvider outline = new XModelObjectContentOutlineProvider();
	private ActivationListener fActivationListener= new ActivationListener();
	protected XModelObjectSelectionProvider selectionProvider = new XModelObjectSelectionProvider();
	protected NatureChecker natureChecker = new NatureChecker();

	private QualifiedName persistentTabQualifiedName = new QualifiedName("", "Selected_tab"); //$NON-NLS-1$ //$NON-NLS-2$
	int selectedPageIndex = 0;
	
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		natureChecker.check(input, getSupportedNatures(), getNatureWarningMessageKey());
		super.init(site, natureChecker.input);
		IWorkbenchWindow window= getSite().getWorkbenchWindow();
		window.getPartService().addPartListener(fActivationListener);
		window.getShell().addShellListener(fActivationListener);
	}
	
	protected void setInput(IEditorInput input) {
		super.setInput(XModelObjectEditorInput.checkInput(input));
		updateFile();
		firePropertyChange(IEditorPart.PROP_INPUT);
	}
	
    protected IEditorSite createSite(IEditorPart editor) {
    	if(editor instanceof ITextEditor) {
    		return new PostMultiPageEditorSite(this, editor);
    	} else {
    		return new MultiPageEditorSite(this, editor);
    	}
    }
	public boolean isAppropriateNature() {
		return natureChecker.isAppropriateNature;
	}
	
	/**
	 * Returns list of natures, of which at least one is necessary to 
	 * open gui tabs of the editor. Null value is equivalent to list of 
	 * all natures implementing IModelNature. Empty array is equivalent 
	 * to turning the check off.  
	 */	
	protected String[] getSupportedNatures() {
		return null;
	}
	
	/**
	 * Returns key for a warning message displayed if project does not 
	 * have a supported nature. 
	 */	
	protected String getNatureWarningMessageKey() {
		return "SharableEditors.natureWarning.message"; //$NON-NLS-1$
	}

	private void updateFile() {
		IFile file = getFile();
		if(file != null) try {
			file.refreshLocal(0, null);
		} catch (CoreException e) {
			//ignore
		}
	}
	
	private IFile getFile() {
		IEditorInput input = getEditorInput();
		return (input instanceof IFileEditorInput) ? ((IFileEditorInput)input).getFile() : null;
	}
	
	private void loadSelectedTab() {
		IFile file = getFile();
		try {
			if("yes".equals(PreferenceModelUtilities.getPreferenceModel().getByPath(Preference.EDITOR_PATH).getAttributeValue("selectSourceTab"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				selectedPageIndex = getSourcePageIndex();
			} else if(file == null) {
				loadSelectedTabForStorage();
			} else {
				String q = file.getPersistentProperty(persistentTabQualifiedName);
				selectedPageIndex = (q == null) ? 0 : Integer.parseInt(q);
			}
		} catch (NumberFormatException e) {
			selectedPageIndex = 0;
		} catch (CoreException e) {
			selectedPageIndex = 0;
		}		
	}
	
	private void loadSelectedTabForStorage() {
		if(object == null) return;
		String path = object.getPath();
		QualifiedName qn = new QualifiedName("", "Selected_tab_" + path); //$NON-NLS-1$ //$NON-NLS-2$
		IProject p = EclipseResourceUtil.getProject(object);
		if(p == null || !p.isOpen()) return;
		try {
			String q = p.getPersistentProperty(qn);
			selectedPageIndex = (q == null) ? 0 : Integer.parseInt(q);
		} catch (NumberFormatException e) {
			//ignore
			selectedPageIndex = 0;			
		} catch (CoreException e) {
			//ignore
			selectedPageIndex = 0;			
		}
	}
	
	private void saveSelectedTab() {
		IFile file = getFile();
			if(file == null) {
				saveSelectedTabForStorage();
			} else {
				try {
					if(file.isAccessible()) file.setPersistentProperty(persistentTabQualifiedName, "" + selectedPageIndex); //$NON-NLS-1$
				} catch (CoreException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}		
			}
	}
	
	private void saveSelectedTabForStorage() {
		if(object == null) return;
		IProject p = EclipseResourceUtil.getProject(object);
		if(p == null || !p.isOpen()) return;
		String path = object.getPath();
		QualifiedName qn = new QualifiedName("", "Selected_tab_" + path); //$NON-NLS-1$ //$NON-NLS-2$
		try {
			p.setPersistentProperty(qn, "" + selectedPageIndex); //$NON-NLS-1$
		} catch (CoreException e) {
			//ignore
		}
	}

	public final boolean isWrongEntity() {
		return getModelObject() != null && isWrongEntity(getModelObject().getModelEntity().getName());
	}
	
	protected boolean isWrongEntity(String entity) {
		return false;
	}
	
	protected XModelObject getModelObject() {
		return (cache == null) ? null : cache.getObject();
	}

	protected void createPages() {
		IEditorInput _input = getEditorInput();
		setPartName(_input.getName());
		if(!(_input instanceof IModelObjectEditorInput)) {
			createUnloadedPage();
			return;
		} 
		input = (IModelObjectEditorInput)getEditorInput();
		object = input.getXModelObject();
		timeStamp = (object == null) ? -1 : object.getTimeStamp();
		lastModifiedTimeStamp = (object == null || object.isModified()) ? -1 : object.getLastModificationTimeStamp();
		cache = new XModelObjectCache(object);
		outline.setCache(cache);
		model = object.getModel();
		getSite().setSelectionProvider(selectionProvider);
		doCreatePages();
		model.addModelTreeListener(syncListener);
		loadSelectedTab();
		if(selectedPageIndex < getPageCount()) {
			setActivePage(selectedPageIndex);
		}
		updateSelectionProvider();
		new ResourceChangeListener(this, getContainer());
	}
	
	public void selectPageByName(String name) {
		if(name == null) return;
		for (int i = 0; i < getPageCount(); i++) {
			String h = getPageText(i);
			if(name.equals(h)) {
				if(selectedPageIndex == i) return;
				selectedPageIndex = i;
				switchToPage(i);
			}
		}
		
	}
	
	protected void createUnloadedPage() {
		createTextPage();
	}
	
	protected void doCreatePages() {
	}
	
	protected void createTreePage() {
		installTreePage(new TreeGuiEditor());
	}
	
	protected void createTextPage() {
		textEditor = createTextEditor();
		try {
			int index = addPage((IEditorPart)textEditor, getEditorInput());
			setPageText(index, ObjectMultiPageEditorMessages.PAGES_EDITOR_SOURCE_TAB); 
			textEditor.setObject(object);
			textEditor.addFocusListener(new TextFocusListener());
			outline.addSelectionChangedListener(new OutlineSelectionListener());
		} catch (PartInitException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
			textEditor = null;
		}
	}
	
	class OutlineSelectionListener implements ISelectionChangedListener {
		boolean isFiringToSource = false;

		public void selectionChanged(SelectionChangedEvent event) {
			if(isFiringToSource) return;
			ISelection s = event.getSelection();
			if(s.isEmpty() || !(s instanceof IStructuredSelection)) return;
///			if(getActivePage() != getSourcePageIndex()) return;
			if(selectionProvider.isFiringSelection() && getActivePage() == getSourcePageIndex()) return;
			if(outline.getControl() == null || outline.getControl().isDisposed()) return;
			boolean isFocused = outline.getControl().isFocusControl();
			if(isFocused) {
				isFiringToSource = true;
				selectionProvider.setSelection(event.getSelection());
				isFiringToSource = false;
				return;
			}
			if(!isFocused && getActivePage() == getSourcePageIndex()) {
				return;
			}			
			Object o = ((IStructuredSelection)s).getFirstElement();
			if(!(o instanceof XModelObject)) return;
			XModelObject so = (XModelObject)o;
			isFiringToSource = true;
			postponedTextSelection.select(so, null);
			isFiringToSource = false;
		}
		
	}
	
	protected final void installTreePage(AbstractSectionEditor treeEditor) {
		this.treeEditor = treeEditor;
		Control control = treeEditor.createControl(getContainer());
		treeEditor.addErrorSelectionListener(createErrorSelectionListener());
		int index = addPage(control);
		setPageText(index, "Tree");
		selectionProvider.addHost("treeEditor", treeEditor.getSelectionProvider()); //$NON-NLS-1$
	}
	
	protected ObjectTextEditor createTextEditor() {
		return null;	
	}

	public boolean isDirty() {
		if(super.isDirty()) return true;
		if(input == null) return false;
		XModelObject o = getModelObject();
		if( (o != null && lastModifiedTimeStamp != o.getLastModificationTimeStamp()) ||
		   (textEditor != null && textEditor.isModified())) return true;
		return false;
	}
	
	public void doSave(IProgressMonitor monitor) {
		if(input == null) {
			if(textEditor != null) textEditor.save();
			return;
		}
		if(!checkReadOnlyOnSave()) {
			if(monitor != null) monitor.setCanceled(true);
			return;
		} 
		if(textEditor != null && textEditor.isModified()) textEditor.save();
		XModelObject o = getModelObject();
		if(o == null || !o.isActive()) return;
		XModelObject p = o.getParent();
		if(!(p instanceof FolderImpl)) return;
		FolderImpl f = (FolderImpl)p;
		
		if (treeFormPage!=null) {
			treeFormPage.doSave(monitor);
		}

		this.enableSanityChecking(false);
		try {
			f.saveChild(o);
		} catch (XModelException e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		this.updateModificationStamp(this.getEditorInput());
		this.enableSanityChecking(true);
		
		if(textEditor != null) textEditor.setModified(false);
		lastModifiedTimeStamp = o.getLastModificationTimeStamp();
		firePropertyChange(IEditorPart.PROP_DIRTY);
		
		saveX(monitor);
		
		f.updateRegistration(o);
	}
	
	
	/**
	 * FIXME remove Java reflection calls
	 * @param monitor
	 */
	void saveX(IProgressMonitor monitor) {
		if(!(textEditor instanceof AbstractTextEditor)) return;
		try {
			Method m = AbstractTextEditor.class.getDeclaredMethod("performSave", new Class[]{boolean.class, IProgressMonitor.class}); //$NON-NLS-1$
			m.setAccessible(true);
			m.invoke(textEditor, new Object[]{Boolean.TRUE, monitor});
		} catch (SecurityException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (NoSuchMethodException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (InvocationTargetException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalAccessException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

	public void doSaveAs() {
		performSaveAs(null);
	}

	public void gotoMarker(IMarker marker) {
		if(marker == null || getModelObject() == null || !marker.exists()) return;
		String path = marker.getAttribute("path", null); //$NON-NLS-1$
		if(path != null) {
			XModelObject o = getModelObject().getModel().getByPath(path);
			if(o == null) return;
			selectionProvider.setSelection(new StructuredSelection(o));
			switchToPage(getSourcePageIndex());

			if(marker.getAttribute(IMarker.LINE_NUMBER, -1) != -1) {
				postponedTextSelection.clean();
				if(textEditor != null) textEditor.gotoMarker(marker);
			} else {
				String attr = marker.getAttribute("attribute", ""); //$NON-NLS-1$ //$NON-NLS-2$
				if(attr == null || attr.length() == 0) {
					postponedTextSelection.select(o, null);
				} else {
					postponedTextSelection.select(o, attr);
				}
			}
		} else {
			int offset = marker.getAttribute(IMarker.CHAR_START, -1);
			int length = marker.getAttribute(IMarker.CHAR_END, -1);	
			if (offset > -1 && length > -1) {
				postponedTextSelection.clean();
				if(textEditor != null) textEditor.gotoMarker(marker);
			}
		}
	}
	
	protected void switchToPage(int page) {
		if (getActivePage() != page && page >= 0) {
			setActivePage(page);
			pageChange(page);
		}
	}
	
	protected boolean isErrorMode() {
		return isErrorMode; 
	}
	
	public boolean isSaveAsAllowed() {
		return true;
	}
	
	public void dispose() {
		saveSelectedTab();
		super.dispose();
		if(model == null) return;
		model.removeModelTreeListener(syncListener);
		model = null;
		if (syncListener!=null) syncListener.dispose();
		syncListener = null;
		if(treeFormPage != null) treeFormPage.dispose();
		if(treeEditor != null) treeEditor.disposeGui();
		if (fActivationListener != null) {
			IWorkbenchWindow window= getSite().getWorkbenchWindow();
			window.getPartService().removePartListener(fActivationListener);
			Shell shell= window.getShell();
			if (shell != null && !shell.isDisposed())
				shell.removeShellListener(fActivationListener);
			fActivationListener= null;
		}
		XModelObject o = getModelObject();
		if(o != null && o.isModified() && o.isActive()) {
			try {
				XAction action = XActionInvoker.getAction("DiscardActions.Discard", object); //$NON-NLS-1$
				if(action != null) {
					// to avoid confirmation
					action.executeHandler(object, null); 
				} else if(object.getParent() instanceof FolderImpl) {
					((FolderImpl)object.getParent()).discardChildFile(object);
				}
 			} catch (XModelException e) {	
 				ModelPlugin.getPluginLog().logError(e);
 			}
		}
		if (outline!=null) outline.dispose();
		outline = null;
	}
	
	protected void initEditors() {
		checkErrorMode();
		if(isErrorMode) {
			setErrorMode();
		} else {
			setNormalMode();
		}
		if(textEditor != null) textEditor.setObject(object);
	}
	
	boolean lock2 = false;
	
	boolean waitForMerge = false;
	int waitingEventsCount = 0;
	
	public void nodeChanged(XModelTreeEvent event) {
		if(lock2) return;
		if(event.getDetails() == XModelTreeEvent.BEFORE_MERGE && event.getModelObject() == getModelObject()) {
			waitForMerge = true;
			waitingEventsCount = 0;
			return;
		}
		if(event.getDetails() == XModelTreeEvent.AFTER_MERGE && event.getModelObject() == getModelObject()) {
			waitForMerge = false;
//			System.out.println("waitingEventsCount=" + waitingEventsCount);
			waitingEventsCount = 0;
		}
		waitingEventsCount++;
		if(waitForMerge) return;
		if(needsUpdate()) {
			Display.getDefault().syncExec(new U());
		}
	}
	
	public void structureChanged(XModelTreeEvent event) {
		if(lock2) return;
		waitingEventsCount++;
		if(waitForMerge) return;
		if(needsUpdate()) {
			Display.getDefault().syncExec(new U());
		}
	}
	
	class U implements Runnable {
		public void run() {
			lock2 = true;
			try {
				update0();
				while(needsUpdate()) {
					update0();
				}
			} finally {
				lock2 = false;
			}
		}
	}
	
	protected boolean needsUpdate() {
		XModelObject o = getModelObject();
		if(o == object && (o == null || o.getTimeStamp() == timeStamp)) {
			if(o != null && o.getLastModificationTimeStamp() != lastModifiedTimeStamp) {
				if(!o.isModified()) lastModifiedTimeStamp = o.getLastModificationTimeStamp();
				firePropertyChange(IEditorPart.PROP_DIRTY);
				if(textEditor != null) textEditor.updateModification();
			}
			return false;
		} 
		object = o;
		timeStamp = (o == null) ? -1 : o.getTimeStamp();
		return true;
	}

	public void update0() {
//		setContentDescription(getEditorInput().getName());
		setPartName(getEditorInput().getName());
		checkErrorMode();
		if(isErrorMode) {
			setErrorMode();
		} else {
			setNormalMode();
		}
		if(textEditor != null) textEditor.updateDocument();
	}	
	
	protected void setErrorMode() {
	}
	
	protected void setNormalMode() {
	}

	protected void checkErrorMode() {
		if(object == null) return;
		boolean i = isWrongEntity() || "yes".equals(object.get("isIncorrect")); //$NON-NLS-1$ //$NON-NLS-2$
		if(isErrorMode == i) return;
		isErrorMode = i;
	}

	public Object getAdapter(Class adapter) {
		if(adapter != null && adapter.isAssignableFrom(IContentOutlinePage.class)) {
			if(input == null) {
				return null;
			}
			return outline;
		} else if(ITextEditor.class == adapter) {
			if(textEditor instanceof ITextEditor) return textEditor;			
		}
		return super.getAdapter(adapter);
	}
	
	class TextFocusListener extends FocusAdapter {
		public void focusLost(FocusEvent e) {
			if(!textEditor.isModified()) return;
			Display.getDefault().syncExec( 
				new Runnable() {
					public void run() {
						try {
							Thread.sleep(200);
						} catch (InterruptedException e) {
							//ignore
						}
						textEditor.save();
					}
				}
			);			
		}
	}
	
	protected ErrorSelectionListener createErrorSelectionListener() {
		 return new ErrorSelectionListenerImpl();
	}
	
	class ErrorSelectionListenerImpl implements ErrorSelectionListener {
		public void errorSelected(int line, int position) {
			doErrorSelected(line, position);
		}		
	}
	
	protected int getSourcePageIndex() {
		return getPageCount() - 1;
	}
	
	void doErrorSelected(int line, int position) {
		setActivePage(getSourcePageIndex());
		pageChange(getSourcePageIndex());
		if(textEditor != null) textEditor.setCursor(line, position);			
	}
	
	public void activateErrorTab() {
		setActivePage(0);
	}
	
	protected void updateEditableMode() {
		
	}
	
	public IEditorPart getActiveEditor() {
		return super.getActiveEditor();
	}
	
	public StructuredTextEditor getSourceEditor() {
		IEditorPart activePageEditor = getActiveEditor();
		if (activePageEditor instanceof StructuredTextEditor) {
			return (StructuredTextEditor)activePageEditor;
		}
		return null;
	}

	TextSelectionProvider textSelectionProvider = new TextSelectionProvider();
	
	protected AbstractSelectionProvider getTextSelectionProvider() {
		textSelectionProvider.init();
		return textSelectionProvider;
	}
	
	private class TextSelectionProvider extends AbstractSelectionProvider implements ISelectionChangedListener {
		boolean inites = false;
		
		public void init() {
			if(inites) return;
			inites = true;
			if(textEditor instanceof TextEditor) {
				((TextEditor)textEditor).getSelectionProvider().addSelectionChangedListener(this);
			}
		}

		protected XModelObject getSelectedModelObject() {
			XModelObject o = textEditor == null ? null : textEditor.findModelObjectAtCursor();
			if(o != null) return o;
			return getModelObject();
		}

		protected void setSelectedModelObject(XModelObject object) {
			postponedTextSelection.select(object, null);
		}

		public void selectionChanged(SelectionChangedEvent event) {
			fireSelectionChanged();
		}
		
		public void dispose() {
			if(textEditor instanceof TextEditor) {
				((TextEditor)textEditor).getSelectionProvider().removeSelectionChangedListener(this);
			}
		}
		
	}

	/**
	 * Internal part and shell activation listener for triggering state validation.
	 * @since 2.0
	 */
	class ActivationListener extends ShellAdapter implements IPartListener {
		private IWorkbenchPart fActivePart;
		private boolean fIsHandlingActivation= false;
		
		public void partActivated(IWorkbenchPart part) {
			fActivePart= part;
			handleActivation();
			if(getActivePage() != getSourcePageIndex() && textEditor != null && textEditor.isModified()) {
				textEditor.save();
			}
		}
	
		public void partBroughtToTop(IWorkbenchPart part) {}
	
		public void partClosed(IWorkbenchPart part) {}
	
		public void partDeactivated(IWorkbenchPart part) {
			fActivePart= null;
		}
	
		public void partOpened(IWorkbenchPart part) {}
	
		public void shellActivated(ShellEvent e) {
			updateEditableMode();
			/*
			 * Workaround for problem described in 
			 * http://dev.eclipse.org/bugs/show_bug.cgi?id=11731
			 * Will be removed when SWT has solved the problem.
			 */
			e.widget.getDisplay().asyncExec(new Runnable() {
				public void run() {
					handleActivation();
					if(getActivePage() != getSourcePageIndex() && textEditor != null && textEditor.isModified()) {
						textEditor.save();
					}
				}
			});
		}
		
		private void handleActivation() {
			if (fIsHandlingActivation)
				return;
				
			if (fActivePart != null && fActivePart.getSite() == getSite()) {
				fIsHandlingActivation= true;
				try {
					doSanityCheckState(getEditorInput());
				} finally {
					fIsHandlingActivation= false;
				}
			}
		}
	};

	private long fModificationStamp= -1;
	private boolean fIsSanityCheckEnabled= true;
	
	protected void enableSanityChecking(boolean enable) {
		synchronized (this) {
			fIsSanityCheckEnabled= enable;
		}
	}

	protected void safelySanityCheckState(IEditorInput input) {
		boolean enabled= false;
		
		synchronized (this) {
			enabled= fIsSanityCheckEnabled;
		}
		
		if (enabled)
			doSanityCheckState(input);
	}

	protected boolean doSanityCheckState(IEditorInput input) {
		if (input == null) return false;

		if (input instanceof IFileEditorInput) {
			IFile iFile = ((IFileEditorInput)input).getFile();
			if (iFile == null) return false;

			File f = (iFile.getLocation() == null ? null : iFile.getLocation().toFile());
			if (f == null) return false;
			
			if (fModificationStamp == -1) 
				fModificationStamp= f.lastModified();

			long stamp= f.lastModified();
			try {
				handleEditorInputChanged();
			} catch (XModelException e) {
				ModelPlugin.getPluginLog().logError(e);
			}
			if (stamp != fModificationStamp) {
				fModificationStamp= stamp;
//				handleEditorInputChanged();
				return true;
			}
		}
		return false;
	}
	
	private void updateModificationStamp(IEditorInput input) {
		try {
			if(input instanceof IFileEditorInput) {
				this.fModificationStamp = ((IFileEditorInput)input).getFile()
					.getLocation().toFile().lastModified();
			}
		} finally {
			this.fModificationStamp = -1;
		}
	}
	
	private void handleEditorInputChanged() throws XModelException {
		XModelObject o = getModelObject();
		if(o == null) return;
		if(input instanceof IFileEditorInput && o.getParent() instanceof FolderImpl) {
			FolderImpl f = (FolderImpl)o.getParent();
			IFile file = ((IFileEditorInput)input).getFile();
			if(file.isSynchronized(IResource.DEPTH_ZERO)) return;
			f.updateChildFile(o, file.getLocation().toFile());
			if(textEditor instanceof ITextEditor) {
				IDocumentProvider provider = ((ITextEditor)textEditor).getDocumentProvider();
				if(provider instanceof IDocumentProviderExtension) {
					IDocumentProviderExtension extension= (IDocumentProviderExtension) provider;
					try {
						extension.synchronize(input);
					} catch (CoreException e) {
						ModelUIPlugin.getDefault().logError(e);
					}
				}
			} else {
				try {
					file.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
				} catch (CoreException e) {
					ModelUIPlugin.getDefault().logError(e);
				}
			}
			f.setModified(false);
			firePropertyChange(ITextEditor.PROP_DIRTY);
		}
	}
	
	PostponedTextSelection postponedTextSelection = new PostponedTextSelection();
	
	class PostponedTextSelection implements Runnable {
		XModelObject selected = null;
		String attribute = null;
		boolean _lock = false;
		
		public void clean() {
			selected = null;
			attribute = null;
		}

		public void select(XModelObject object, String attr) {
			if(textEditor == null || _lock) return;
			if(getActivePage() == getSourcePageIndex()) {
				_lock = true;
				try {
					textEditor.selectModelObject(object, attr);
				} finally {
					_lock = false;
				}
			} else {
				selected = object;
				attribute = (attr == null || attr.length() == 0) ? null : attr;
			}
		}
		public void run() {
			if(selected == null || textEditor == null) return;
			if(_lock) return;
			_lock = true;
			try {
				textEditor.selectModelObject(selected, attribute);
			} finally {
				_lock = false;
			}
			clean();
		}
	}

	protected void pageChange(int newPageIndex) {
		deactivateSite(false, false);
		boolean isText = selectedPageIndex == getSourcePageIndex();
		selectedPageIndex = newPageIndex;
		Control control = getControl(newPageIndex);
		if (control != null) {
			control.setVisible(true);
		}
		setFocus();
		IEditorPart activeEditor = getEditor(newPageIndex);
		IEditorActionBarContributor contributor = getEditorSite().getActionBarContributor();
		if(contributor instanceof EditorActionBarContributorWrapper)
		  contributor = ((EditorActionBarContributorWrapper)contributor).getActiveContributer();
		if (contributor instanceof MultiPageEditorActionBarContributor) {
			((MultiPageEditorActionBarContributor) contributor).setActivePage(activeEditor);
		}
		updateSelectionProvider();
		if(postponedTextSelection.selected != null) {
			Display.getDefault().asyncExec(postponedTextSelection);
		}
		if(isText && newPageIndex != getSourcePageIndex()) {
			synchronizeSelectionWithText();
		}
		activateSite();
	}

	protected void synchronizeSelectionWithText() {
	}

	protected void updateSelectionProvider() {
	}
	
	protected boolean checkReadOnlyOnSave() {
		IFile f = getFile();
		if(f == null || !f.exists() || !f.isReadOnly()) return true;
		String title= "Save problems";
		String msg= " Cannot could not be completed.";
		IStatus status = new Status(Status.ERROR, ModelUIPlugin.PLUGIN_ID, Status.OK, MessageFormat.format("File {0} is read-only.", f.getLocation().toString()), new Exception());
		ErrorDialog.openError(getSite().getShell(), title, msg, status);
		return false;
	}
	
//@S_CLASS@

  protected TreeFormPage createTreeFormPage() {
	  treeFormPage = new TreeFormPage();
	  treeFormPage.setLabel("Tree");
	  treeFormPage.setTitle("%TreeFormPage%"); //$NON-NLS-1$
	  treeFormPage.addErrorSelectionListener(createErrorSelectionListener());
	  return treeFormPage;
  }

  protected void addFormPage(TreeFormPage formPage) {
	  try {
		  int index = addPage(formPage, getEditorInput());
		  setPageText(index, formPage.getLabel());
		  selectionProvider.addHost("treeEditor", formPage.getSelectionProvider()); //$NON-NLS-1$
		  //Activate key binding service here
		  formPage.getEditorSite().getKeyBindingService();
	  } catch (PartInitException ex) {
		  ModelUIPlugin.getPluginLog().logError(ex);
	  }
		  //getSite().setSelectionProvider(formPage.getSelectionProvider());
  }
/*
  protected void addFormPage(IFormPage formPage) {
	  Control control = formPage.createControl(getContainer());
	  int index = addPage(control);
	  setPageText(index, formPage.getLabel());
	  selectionProvider.addHost("treeEditor", formPage.getSelectionProvider());
	  //getSite().setSelectionProvider(formPage.getSelectionProvider());
  }
*/

	protected void performSaveAs(IProgressMonitor progressMonitor) {
		Shell shell = getSite().getShell();
		SaveAsDialog dialog = new SaveAsDialog(shell);
		initSaveAsDialog(dialog);
		dialog.create();

		final IFile file = runSaveAsDialog(dialog, progressMonitor);
		if (file == null) return;
		
		WorkspaceModifyOperation op = new WorkspaceModifyOperation() {
			public void execute(final IProgressMonitor monitor) throws CoreException {
				String body = ((FileAnyImpl)object).getAsText();
				ByteArrayInputStream b = new ByteArrayInputStream(body.getBytes());
				if(!file.exists()) {
					file.create(b, true, monitor);
				} else {
					file.setContents(b, true, false, monitor);
				}
				object.getModel().update();
				file.getParent().refreshLocal(IResource.DEPTH_INFINITE, monitor);
			}
		};		
		boolean success = false;
		try {			
			new ProgressMonitorDialog(shell).run(false, true, op);
			success = true;			
		} catch (InterruptedException x) {
			//ignore
		} catch (InvocationTargetException x) {
			Throwable targetException= x.getTargetException();
			String title = "Save As";
			String msg = "Error: " + targetException.getMessage();			
			if (targetException instanceof CoreException) {
				CoreException coreException= (CoreException) targetException;
				IStatus status= coreException.getStatus();
				if (status != null) {
					switch (status.getSeverity()) {
						case IStatus.INFO:
							MessageDialog.openInformation(shell, title, msg);
							break;
						case IStatus.WARNING:
							MessageDialog.openWarning(shell, title, msg);
							break;
						default:
							MessageDialog.openError(shell, title, msg);
					}
				} else {
					 MessageDialog.openError(shell, title, msg);
				}
			}
		} finally {
			if (success) {
				XModelObject o = null;
				for (int i = 0; i < 5 && o == null; i++) {
					o = EclipseResourceUtil.getObjectByResource(file);
					if(o == null) try {
						Thread.sleep(200);
					} catch (InterruptedException e) {
						//ignore
					}
				}
				if(o == null) o = EclipseResourceUtil.createObjectForResource(file);
				if(o != null) {
					XActionInvoker.invoke("Open", o, null); //$NON-NLS-1$
				}
			} 
		}		
		if (progressMonitor != null) progressMonitor.setCanceled(!success);
	}
	private void initSaveAsDialog(SaveAsDialog dialog) {
		IFile original = (input instanceof IFileEditorInput) ? ((IFileEditorInput) input).getFile() : null;
		if (original != null) dialog.setOriginalFile(original);		
	}
	private IFile runSaveAsDialog(SaveAsDialog dialog, IProgressMonitor progressMonitor) {
		if (dialog.open() == Dialog.CANCEL) {
			if (progressMonitor != null) progressMonitor.setCanceled(true);
			return null;
		}			
		IPath filePath = dialog.getResult();
		if (filePath == null) {
			if (progressMonitor != null) progressMonitor.setCanceled(true);
			return null;
		}
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		return workspace.getRoot().getFile(filePath);
	}
	
	void updateTitle() {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				setPartName(getEditorInput().getName());
			}
		});
	}
	
	class PostMultiPageEditorSite extends MultiPageEditorSite {
		
		private ISelectionChangedListener postSelectionChangedListener = null;

		public PostMultiPageEditorSite(ObjectMultiPageEditor multiPageEditor, IEditorPart editor) {
			super(multiPageEditor, editor);
		}

		private ISelectionChangedListener getPostSelectionChangedListener() {
			if (postSelectionChangedListener == null) {
				postSelectionChangedListener = new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						PostMultiPageEditorSite.this.handlePostSelectionChanged(event);
					}
				};
			}
			return postSelectionChangedListener;
		}

		protected void handlePostSelectionChanged(SelectionChangedEvent event) {
			if(selectedPageIndex != getSourcePageIndex()) {
				//link outline only to text editor
				return;
			}
			ISelectionProvider parentProvider = getMultiPageEditor().getSite().getSelectionProvider();
			ISelection s = event.getSelection();
			if(s == null || s.isEmpty()) return;
			if(s instanceof ITextSelection) {
				XModelObject o = textEditor.findModelObjectAtCursor();
				if(o != null) {
					SelectionChangedEvent newEvent = new SelectionChangedEvent(parentProvider, new StructuredSelection(o));
					if(parentProvider instanceof XModelObjectSelectionProvider) {
						((XModelObjectSelectionProvider)parentProvider).postSelectionChanged(newEvent);
					}
				}
			}
		}

		public void setSelectionProvider(ISelectionProvider provider) {
			ISelectionProvider oldSelectionProvider = getSelectionProvider();
			if (oldSelectionProvider != null) {
				if (oldSelectionProvider instanceof IPostSelectionProvider) {
					((IPostSelectionProvider) oldSelectionProvider).removePostSelectionChangedListener(getPostSelectionChangedListener());
				}
			}

			super.setSelectionProvider(provider);

			if (provider != null) {
				if (provider instanceof IPostSelectionProvider) {
					((IPostSelectionProvider) provider).addPostSelectionChangedListener(getPostSelectionChangedListener());
				}
			}
		}

	}
}

class ResourceChangeListener implements IResourceChangeListener {
	IEditorPart editorPart;
	Composite container;
	
	ResourceChangeListener(IEditorPart editorPart, Composite container) {
		this.editorPart = editorPart;
		this.container = container;
		IWorkspace workspace = ModelUIPlugin.getWorkspace();
		if (workspace == null) return;
		workspace.addResourceChangeListener(this);
		container.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				IWorkspace workspace = ModelUIPlugin.getWorkspace();
				if (workspace == null) return;
				workspace.removeResourceChangeListener(ResourceChangeListener.this);
			}
		});
	}

	public void resourceChanged(IResourceChangeEvent event) {
		IEditorInput ei = editorPart.getEditorInput();

		if(ei instanceof IModelObjectEditorInput) {
			XModelObject o = ((IModelObjectEditorInput)ei).getXModelObject();
			IProject project = EclipseResourceUtil.getProject(o);
			if(project != null && (!project.exists() || !project.isOpen())) {
				closeEditor();
				return;
			}
		}
		
		if(!(ei instanceof IFileEditorInput)) return;
		IFileEditorInput fi = (IFileEditorInput)ei;
		IFile f = fi.getFile();
		if(f == null) return;
		IPath path = getPathChange(event, f);
		if(path == null) {
			if(f != null && !f.exists()) closeEditor();
			return;
		}
		f = ModelPlugin.getWorkspace().getRoot().getFile(path);
		XModelObject p = f == null ? null : EclipseResourceUtil.getObjectByResource(f.getParent());
		if(p instanceof FolderImpl) {
			((FolderImpl)p).update();
		}
		XModelObject o = EclipseResourceUtil.getObjectByResource(f);
		if(f != null && f.exists() && o != null) {
			if(editorPart instanceof ObjectMultiPageEditor) {
				ObjectMultiPageEditor e = (ObjectMultiPageEditor)editorPart;
				if(ei instanceof XModelObjectEditorInput) {
					IEditorInput e2 = XModelObjectEditorInput.createInstance(o);
					e.setInput(e2);
					e.updateTitle();
					if(e.textEditor instanceof AbstractTextEditor) {
						((AbstractTextEditor)e.textEditor).setInput(e2);
						((XModelObjectEditorInput)ei).synchronize();
						if(((XModelObjectEditorInput)ei).getXModelObject() != o) {
							closeEditor();
							return;
						}
					}
				}
			}
		}
		if(f == null || f.exists()) return;
		closeEditor();
	}
	
	private void closeEditor() {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				editorPart.getSite().getPage().closeEditor(editorPart, false);
			}
		});
	}
	
	private IPath getPathChange(IResourceChangeEvent event, IFile f) {
		return getPathChange(event.getDelta(), f.getFullPath());
	}

	private IPath getPathChange(IResourceDelta delta, IPath p) {
		if(delta == null || delta.getFullPath() == null) return null;
		if(!delta.getFullPath().isPrefixOf(p)) return null;
		if(delta != null && delta.getKind() == IResourceDelta.CHANGED) {
			IResourceDelta[] ds = delta.getAffectedChildren();
			if(ds == null) return null;
			if(ds.length > 1) {
				return getPathChange(ds, p);
			}
			for (int i = 0; i < ds.length; i++) {
				IPath ps = getPathChange(ds[i], p);
				if(ps != null) return ps;
			}
		}
		return null;
	}
	
	private IPath getPathChange(IResourceDelta[] ds, IPath p) {
		int index = -1;
		boolean equals = false;
		IPath dp = null;
		for (int i = 0; i < ds.length; i++) {
			if(ds[i].getKind() == IResourceDelta.REMOVED) {
				IPath d = ds[i].getFullPath();
				if(d.equals(p)) {
					equals = true;
					index = i;
					dp = d;
					break;
				} else if(d.isPrefixOf(p)) {
					index = i;
					dp = d;
				}				
			}			
		}
		if(index < 0) return null;
		for (int i = 0; i < ds.length; i++) {
			if(ds[i].getKind() == IResourceDelta.ADDED) {
				IPath d = ds[i].getFullPath();
				IPath df = ds[i].getMovedFromPath();
				if(!dp.equals(df)) continue;
				if(equals) return d;
				return d.append(p.removeFirstSegments(dp.segmentCount()));
			}			
		}
		return null;
	}
}

class NatureChecker {
	boolean isAppropriateNature = false;
	IResource resource = null;
	String[] natures;
	String warningKey;
	IEditorInput input;
	
	public boolean isAppropriateNature() {
		return isAppropriateNature;
	}
	
	public void check(IEditorInput input, String[] natures, String warningKey) {
			//Suppress check: EXIN-292
			natures = new String[0];
		
		this.input = input;
		if(input instanceof IFileEditorInput) {
			resource = ((IFileEditorInput)input).getFile();
		} else {
			isAppropriateNature = true;
			return;
		}
		this.warningKey = warningKey;
		this.natures = natures;
		isAppropriateNature = isAppropriateNature(input);
		if(!isAppropriateNature) {
//Suppress check: EXIN-292
//			showWarning();
//			if(this.input instanceof IFileEditorInput) {
//				resource = ((IFileEditorInput)this.input).getFile();
//			}
//			isAppropriateNature = isAppropriateNature(this.input);
		}
	}
	
	private boolean isAppropriateNature(IEditorInput input) {
		input = XModelObjectEditorInput.checkInput(input);
		if(!(input instanceof IModelObjectEditorInput)) return false;
		XModelObject o = ((IModelObjectEditorInput)input).getXModelObject();
		IProject project = (IProject)o.getModel().getProperties().get("project"); //$NON-NLS-1$
		if(project == null) return false;
		if(natures != null && natures.length == 0) return true;
		IModelNature n = EclipseResourceUtil.getModelNature(project);
		if(n == null) return false;
		if(natures == null) return true;
		for (int i = 0; i < natures.length; i++) {
			if(EclipseResourceUtil.getModelNature(project, natures[i]) != null) return true;
		}
		return false;
	}
	
}

