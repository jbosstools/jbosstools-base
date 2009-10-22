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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
//import org.jboss.tools.common.core.jdt.Messages;
import org.jboss.tools.common.editor.form.RightFormContainer;
import org.jboss.tools.common.editor.form.SampleErrorForm;
import org.jboss.tools.common.editor.form.SampleTreeForm;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.XModelTreeListenerSWTASync;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;

import org.jboss.tools.common.meta.action.XActionItem;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.model.XFilteredTreeConstraint;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.forms.DefaultFormFactory;
import org.jboss.tools.common.model.ui.forms.FormFactory;
import org.jboss.tools.common.model.ui.forms.IFormFactory;
import org.jboss.tools.common.model.ui.forms.LayouredFormFactory;
import org.jboss.tools.common.model.ui.forms.XModelObjectFormFactory;
import org.jboss.tools.common.model.ui.forms.DefaultFormContainer;
import org.jboss.tools.common.model.ui.forms.DefaultFormPage;
import org.jboss.tools.common.model.ui.forms.IForm;
import org.jboss.tools.common.model.ui.forms.MementoDOM;
import org.jboss.tools.common.model.ui.forms.SplitFormContainer;
import org.jboss.tools.common.model.ui.resources.ResourceLayoutManager;
import org.jboss.tools.common.model.ui.texteditors.TextActionHelper;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class TreeFormPage extends DefaultFormPage implements ITextEditor, ITextOperationTarget, ISelectionChangedListener {
	private SelectionNotifier selectionNotifier; 
	private XModelObject installedObject;
	private XModel model;
	private boolean errorMode;
	private Map<String,IAction> actions = new HashMap<String,IAction>();
	private ArrayList<String> actionMapping = new ArrayList<String>();
	private IEditorSite site; 
	private IEditorInput input;

	private DefaultFormContainer mainContainer;
	private SplitFormContainer splitContainer;
	private SampleTreeForm treeForm;
	private SampleErrorForm errorForm;
	private RightFormContainer rightFormContainer;
	private XModelTreeListenerSWTASync modelListener  = new XModelTreeListenerSWTASync(new XMTL());

	public TreeFormPage() {
		GridLayout layout = new GridLayout();
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginHeight = 10;
		layout.marginWidth = 10;

		selectionNotifier = new SelectionNotifier();
		selectionNotifier.addSelectionChangedListener(this);
		this.setSelectionProvider(selectionNotifier);
		
		errorForm = new SampleErrorForm();
		errorForm.setVisible(Boolean.FALSE.booleanValue());
		errorForm.setLayout(layout); 
		
		treeForm = new SampleTreeForm(this);
		treeForm.setSelectionNotifier(selectionNotifier);
		rightFormContainer = new RightFormContainer();
		rightFormContainer.setSelectionChangedListener(selectionNotifier);
		
		layout = new GridLayout();
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginHeight = 5;
		layout.marginWidth = 5;
		splitContainer = new SplitFormContainer(treeForm, rightFormContainer);
		splitContainer.setLayout(layout);
		splitContainer.setLayoutData(new GridData(GridData.FILL_BOTH));

		mainContainer = new DefaultFormContainer();
		mainContainer.setLayoutData(new GridData(GridData.FILL_BOTH));
		mainContainer.addForm(errorForm);
		mainContainer.addForm(splitContainer);
		
		this.form = mainContainer;	
	}
	
	public void addErrorSelectionListener(ErrorSelectionListener listener) {
		errorForm.addErrorSelectionListener(listener);
	}
	
	public void addFilter(XFilteredTreeConstraint filter) {
		treeForm.addFilter(filter);
	}
	
	public void initialize(Object model) {
		this.installedObject = (XModelObject)model;
		this.model = installedObject.getModel(); 
		treeForm.initialize(model);
		// add model listener
	}
	
	public Control createFormControl(Composite parent, IWidgetSettings settings) {
		final XModel m = model;
		m.addModelTreeListener(modelListener);
		Control c = super.createFormControl(parent, settings);
		c.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				m.removeModelTreeListener(modelListener);
			}
		});
		return c;
	}

	public void update() {
		if(control == null || control.isDisposed()) return;
		if (treeForm!=null) treeForm.update();
		if (rightFormContainer!=null) rightFormContainer.update();
	}

	public ISelectionProvider getSelectionProvider() {
		return selectionNotifier;
	}

	private void printActionList(String level, XActionList actionList) {
		if(!ModelUIPlugin.getDefault().isDebugging()) return;

		String actionListName = actionList.getName();
		String actionListDisplayName = actionList.getDisplayName();
		String actionListPath = actionList.getPath();
		// TBD switch to trace support
		// System.out.println(level+"ActionList ["+actionListName+"] ["+actionListDisplayName+"] ["+actionListPath+"]");
		XActionItem[] items = actionList.getActionItems();
		for (int i=0;i<items.length;++i) {
			if (items[i] instanceof XActionList) {
				printActionList(level+"    ", (XActionList)items[i]);				 //$NON-NLS-1$
			} else {
				String actionItemName = items[i].getName();
				String actionItemDisplayName = items[i].getDisplayName();
				String actionItemPath = items[i].getPath();
				// TBD switch to trace support
				// System.out.println(level+"    "+"ActionItem ["+actionItemName+"] ["+actionItemDisplayName+"] ["+actionItemPath+"]");
			}
		}
	}
	
	private MementoDOM memento;

	private XModelObject selection = null;

	// ISelectionChangedListener
	public void selectionChanged(SelectionChangedEvent event) {
		XModelObject xmo = getModelObject(event.getSelection());
		if(selection == xmo) return;
		selection = xmo;
		 
		if (xmo!=null && xmo.getModelEntity()!=null) printActionList("", xmo.getModelEntity().getActionList()); //$NON-NLS-1$
		IForm form = (xmo == null) ? null : getFormFactory(xmo).getForm();

		// store form into memento
		if (rightFormContainer.size()>0) {
			if (memento!=null) {
				Iterator i = rightFormContainer.iterator();
				while (i.hasNext()) {
					IForm iForm = (IForm)i.next();
					IMemento formMemento = memento.getChild(iForm.getHeadingText());
					if (formMemento==null) formMemento = memento.createChild(iForm.getHeadingText());
					iForm.store(formMemento);
				}
			}
		}
		rightFormContainer.clear();
		if(form != null) {
			form.initialize(xmo);
			// load form from memento
			if (memento!=null) {
				IMemento formMementro = memento.getChild(form.getHeadingText());
				if (formMementro==null) formMementro = memento.createChild(form.getHeadingText());
				form.load(formMementro);
			}
			rightFormContainer.addForm(form);
			form.setParent(rightFormContainer);
		}
	}

	private IFormFactory getFormFactory(XModelObject selected) {
		if(selected == null) return null;
		XModelObjectFormFactory formFactory = null;
		String formFactoryClassName = selected.getModelEntity().getProperty("formFactory"); //$NON-NLS-1$
		
		if(formFactoryClassName != null) {
			if("%Default%".equals(formFactoryClassName)) { //$NON-NLS-1$
				return new LayouredFormFactory(selected);
			}
			Class cls = ModelFeatureFactory.getInstance().getFeatureClass(formFactoryClassName);
			if(cls == null) return new FormFactory(selected);
			try {
				Constructor c = cls.getConstructor(new Class[]{XModelObject.class});
				formFactory = (XModelObjectFormFactory)c.newInstance(new Object[]{selected}); 
			} catch (InstantiationException e) {
				ModelUIPlugin.getPluginLog().logError(e);	
			} catch (SecurityException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} catch (NoSuchMethodException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} catch (IllegalArgumentException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} catch (IllegalAccessException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} catch (InvocationTargetException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
			return formFactory == null ? new FormFactory(selected) : formFactory;
		} else {
			formFactory = new DefaultFormFactory(selected);
		}

		return formFactory;
	}

	private XModelObject getModelObject(ISelection selection) {
		if(selection == null || selection.isEmpty()) return null;
		IStructuredSelection s = (IStructuredSelection)selection;
		Object o = s.getFirstElement();
		return (o instanceof XModelObject) ? (XModelObject)o : null;
	}
	
	class XMTL implements XModelTreeListener {
		public void nodeChanged(XModelTreeEvent event) {
			XModelObject o = event.getModelObject();
			String p1 = "" + installedObject.getPath() + "/"; //$NON-NLS-1$ //$NON-NLS-2$
			String p2 = "" + o.getPath() + "/"; //$NON-NLS-1$ //$NON-NLS-2$
			if(p2.startsWith(p1)) {
				update();
			}
		}
   	 
		public void structureChanged(XModelTreeEvent event) {
			XModelObject o = event.getModelObject();
			String p1 = "" + installedObject.getPath() + "/"; //$NON-NLS-1$ //$NON-NLS-2$
			String p2 = "" + o.getPath() + "/"; //$NON-NLS-1$ //$NON-NLS-2$
			if(p2.startsWith(p1)) {
				update();
			}
		}
	}

	public boolean isErrorMode() {
		return errorMode;
	}

	public void setErrorMode(boolean b) {
		errorMode = b;
		if (this.installedObject!=null) doErrorMode();
		mainContainer.setEnabled(!b);
	}
	
	private void doErrorMode() {
		if (isErrorMode()) {
			// show error page
			errorForm.initialize(getErrors());
			errorForm.setVisible(Boolean.TRUE.booleanValue());
		} else {
			// hide error page
			//errorForm.initialize(this.installedObject.get("errors"));
			errorForm.setVisible(Boolean.FALSE.booleanValue());
		}
	}
	
	private String getErrors() {
		XModelObject f = installedObject;
		while(f != null && f.getFileType() != XModelObject.FILE) {
			f = f.getParent();
		}
		if(f == null) f = installedObject;
		return f.get("errors"); //$NON-NLS-1$
	}

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
		return (IAction)actions.get(id);
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

	public IEditorInput getEditorInput() {
		return input;
	}

	public IEditorSite getEditorSite() {
		return site;
	}

	public void gotoMarker(IMarker marker) {
	}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		this.site = site;
		this.input = input;
		if (input instanceof IFileEditorInput) {
			memento = new MementoDOM(ResourceLayoutManager.getDefault().getLayoutElement(getFile(), "TreeFormPage")); //$NON-NLS-1$
		}
	}

	public void addPropertyListener(IPropertyListener listener) {
	}

	public void createPartControl(Composite parent) {
		super.createControl(parent);
		createActions();
	}

	public IWorkbenchPartSite getSite() {
		return site;
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
		if(control != null && !control.isDisposed()) control.setFocus();
	}

	public Object getAdapter(Class adapter) {
		if (ITextOperationTarget.class.equals(adapter))	return this;
		return null;
	}
	
	private IFile getFile() {
		return (input instanceof IFileEditorInput) ? ((IFileEditorInput)input).getFile() : null;
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
//		Collection actions = this.actions.values();
//		Iterator i = actions.iterator();
//		IAction action;
//		while (i.hasNext()) {
//			action = (IAction)i.next();
//		}
		return true;
	}

	public void doOperation(int operation) {
		if (operation>actionMapping.size()) {
			ModelUIPlugin.getPluginLog().logError(new IllegalArgumentException("Can not find global action with index: "+operation)); //$NON-NLS-1$
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
		if(treeForm.doGlobalAction(action)) {
			return;
		}
		if(mainContainer.doGlobalAction(action)) {
			// TODO
			// refresh
		}
	}
}