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
package org.jboss.tools.common.propertieseditor;

import java.util.*;

import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.texteditor.*;

import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
//import org.jboss.tools.common.core.jdt.Messages;
import org.jboss.tools.common.model.ui.action.*;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.viewers.*;
import org.jboss.tools.common.model.ui.objecteditor.*;
import org.jboss.tools.common.model.ui.texteditors.TextActionHelper;

public class PropertiesEditor extends XChildrenEditor implements ITextEditor, ITextOperationTarget {
	private ArrayList<String> actionMapping = new ArrayList<String>();
	private Map<String,IAction> actions = new HashMap<String,IAction>();
	private IEditorInput input;
	IEditorSite site;
	
	public PropertiesEditor() {
		xtable.setMultiSelected();
		setMnemonicEnabled(true);
	}

	protected AbstractTableHelper createHelper() {
		return new FPTableHelper();
	}

	protected int[] getColumnWidthHints() {
		return new int[]{10, 20};
	}
	
	protected boolean areUpDounActionsEnabled() {
		return true;
	}

	public Control createControl(Composite parent) {
		super.createControl(parent);
		TMenuInvoker menu = new TMenuInvoker();
		menu.setViewer(xtable.getViewer()); 
		xtable.getViewer().getTable().addMouseListener(menu);
		getControl().addMouseListener(menu);
		return getControl();	
	}
	
	protected void setMargins(CommandBar bar) {
		bar.getLayout().setMargins(10,10,0,10);
	}

	protected Color getItemColor(int i) {
		XModelObject o = helper.getModelObject(i);
		boolean disabled = "no".equals(o.getAttributeValue("enabled"));
		return ((disabled) ? GREYED_COLOR : DEFAULT_COLOR);
	}

	protected String getAddActionPath() {
		return "CreateActions.CreateProperty";
	}

	protected void edit() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o != null) callAction(o, "Properties.Edit");
	}
	
	public class TMenuInvoker extends XMenuInvoker {

		public XModelObject getSelectedModelObject() {
			int i = ((TableViewer)viewer).getTable().getSelectionIndex();
			if(i < 0) return null;
			return helper.getModelObject(i);
		}
	
	}

	///ITextEditor
	public IDocumentProvider getDocumentProvider() {
		return null;
	}

	public void close(boolean save) {}

	public boolean isEditable() {
		return false;
	}

	public void doRevertToSaved() {}

	public void setAction(String actionID, IAction action) {
		actions.put(actionID, action);
	}

	public IAction getAction(String actionId) {
		return actions.get(actionId);
	}

	public void setActionActivationCode(String actionId, char activationCharacter, int activationKeyCode, int activationStateMask) {
	}

	public void removeActionActivationCode(String actionId) {}

	public boolean showsHighlightRangeOnly() {
		return false;
	}

	public void showHighlightRangeOnly(boolean showHighlightRangeOnly) {}

	public void setHighlightRange(int offset, int length, boolean moveCursor) {}

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

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		this.site = site;
		this.input = input;
	}

	public void addPropertyListener(IPropertyListener listener) {}

	public void createPartControl(Composite parent) {
		super.createControl(parent);
		createActions();
	}

	public IWorkbenchPartSite getSite() {
		return site;
	}

	public String getTitle() {
		return null;
	}

	public Image getTitleImage() {
		return null;
	}

	public String getTitleToolTip() {
		return null;
	}

	public void removePropertyListener(IPropertyListener listener) {}

	public void setFocus() {
		try {
			if(xtable == null || !xtable.isActive()) return;
			xtable.getTable().setFocus();
		} catch(Exception e) {
			//ignore
		}
	}

	public Object getAdapter(Class adapter) {
		if (ITextOperationTarget.class.equals(adapter))	return this;
		return null;
	}

	public void doSave(IProgressMonitor monitor) {}

	public void doSaveAs() {}

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
			ModelUIPlugin.getPluginLog().logError(new RuntimeException("Can not find global action with index: "+operation));
		} else {
			String globalAction = (String)actionMapping.get(operation);
			doGlobalAction(globalAction);
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

	public void doGlobalAction(String actionName) {
		if(ITextEditorActionConstants.DELETE.equals(actionName)) {
			action(XChildrenEditor.DELETE);
		} else if(ITextEditorActionConstants.COPY.equals(actionName)) {
			doXActionCopy();
		} else if(ITextEditorActionConstants.PASTE.equals(actionName)) { 
			doXActionPaste();
		}
	}
	
	private void doXActionCopy() {
		ISelection selection = getSelectionProvider().getSelection();
		if(selection == null || selection.isEmpty() || !(selection instanceof StructuredSelection)) return;
		StructuredSelection ss = (StructuredSelection)selection;
		if(!(ss.getFirstElement() instanceof XModelObject)) return;
		XModelObject object = (XModelObject)ss.getFirstElement();
		XModelObject[] os = null;
		if(ss.size() > 1) {
			os = new XModelObject[ss.size()];
			Iterator it = ss.iterator();
			for (int i = 0; i < os.length; i++) os[i] = (XModelObject)it.next(); 
		}
		invokeXAction(XAction.COPY, object, os);
	}

	private void doXActionPaste() {
		invokeXAction(XAction.PASTE, helper.getModelObject(), null);
	}

	private void invokeXAction(String actionPath, XModelObject object, XModelObject[] os) {
		XAction action = XActionInvoker.getAction(actionPath, object);
		if(action == null) return;
		if(os == null) {
			if(!action.isEnabled(object)) return;
			XActionInvoker.invoke(actionPath, object, new Properties());
		} else {
			if(!action.isEnabled(object, os)) return;
			XActionInvoker.invoke(actionPath, object, os, new Properties());
		}
	}

}

class FPTableHelper extends AbstractTableHelper {
	public FPTableHelper() {}

	public String[] getHeader() {
		return new String[]{"name", "value"};
	}
}
