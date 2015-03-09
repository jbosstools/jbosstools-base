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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.editor.AbstractSelectionProvider;
import org.jboss.tools.common.editor.ObjectMultiPageEditor;
import org.jboss.tools.common.editor.ObjectTextEditor;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.propertieseditor.text.PropertiesTextEditor;

public class PropertiesCompoundEditor extends ObjectMultiPageEditor {

	public static boolean isPropertiesFile(XModelObject object) {
		return object != null && XModelObjectConstants.ENTITY_FILE_PROPERTIES.equals(object.getModelEntity().getName());
	}

    protected PropertiesEditor propertiesEditor;

	public void dispose() {
		super.dispose();
		if (propertiesEditor!=null) propertiesEditor.dispose();
		propertiesEditor = null;
	}

	protected void doCreatePages() {
		createPropertiesPage();
		createTextPage();
		initEditors();
	}

	public boolean isDirty() {
		return super.isDirty() || (propertiesEditor != null && propertiesEditor.isDirty());
	}
	
	protected void createPropertiesPage() {
		propertiesEditor = new PropertiesEditor();
		propertiesEditor.setObject(object);
		propertiesEditor.changeListener = new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				firePropertyChange(PROP_DIRTY);				
			}
		};
		int index = -1;
		try {
			propertiesEditor.init(getEditorSite(), getEditorInput());
			index = addPage(propertiesEditor, getEditorInput());
		} catch (PartInitException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return;
		}
//		Control control = propertiesEditor.createControl(getContainer());
//		int index = addPage(control);
		setPageText(index, "Properties"); 
		if(!isPropertiesFile(getModelObject())) return;
		propertiesEditor.update();
		propertiesEditor.refresh();
	}
	
	protected ObjectTextEditor createTextEditor() {
		return new PropertiesTextEditor();	
	}

	protected void setNormalMode() {
		if (treeFormPage!=null) { // AU added
			treeFormPage.initialize(getModelObject()); // AU added
			treeFormPage.setErrorMode(isErrorMode());
		} // AU added
		if(propertiesEditor == null) return;
		if(isPropertiesFile(getModelObject())) {
			propertiesEditor.setObject(object);
			propertiesEditor.update();
		}
		updateSelectionProvider();
	}
	
	protected void checkErrorMode() {}
	
	protected void updateEditableMode() {
		if(propertiesEditor != null) propertiesEditor.update();
	}
	
	public Object getAdapter(Class cls) {
		return super.getAdapter(cls);
	}

	protected void updateSelectionProvider() {
		if(propertiesEditor != null) selectionProvider.addHost("guiEditor", propertiesEditor.getSelectionProvider()); //$NON-NLS-1$
		if(textEditor != null) selectionProvider.addHost("textEditor", getTextSelectionProvider()); //$NON-NLS-1$
		int index = getActivePage();
		if(index == getSourcePageIndex()) {
			if(textEditor != null) {
				selectionProvider.setHost(getTextSelectionProvider());
			}
			return;
		}
		if(propertiesEditor == null || propertiesEditor.getSelectionProvider() == null) {
			/*
			if (treeEditor != null) {
				selectionProvider.setHost(treeEditor.getSelectionProvider());
				treeEditor.fireEditorSelected();
			}
			if (treeFormPage != null) {
				selectionProvider.addHost("treeEditor", treeFormPage.getSelectionProvider(), true);
			}
			*/
		} else {
			ISelectionProvider p = propertiesEditor.getSelectionProvider();
			selectionProvider.setHost(p);
			if(p instanceof AbstractSelectionProvider) {
				((AbstractSelectionProvider)p).fireSelectionChanged();
			}		
		}
	}
	
	protected String[] getSupportedNatures() {
		return new String[0];
	}

	protected void synchronizeSelectionWithText() {
		if(getTextSelectionProvider() == null || propertiesEditor == null) return;
		ISelection s = getTextSelectionProvider().getSelection();
		if(s == null || s.isEmpty() || !(s instanceof IStructuredSelection)) return;
		Object o = ((IStructuredSelection)s).getFirstElement();
		if(!(o instanceof XModelObject) || o == getModelObject()) return;
		propertiesEditor.getSelectionProvider().setSelection(s);
		if(outline != null) outline.setSelection(s);
	}
	
}
