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

import org.jboss.tools.common.editor.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.propertieseditor.text.*;
import org.eclipse.jface.viewers.ISelectionProvider;

public class PropertiesCompoundEditor extends ObjectMultiPageEditor {
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
	
	protected void createPropertiesPage() {
		if(getModelObject() == null) return;
		propertiesEditor = new PropertiesEditor();
		int index = -1;
		try {
			propertiesEditor.init(getEditorSite(), getEditorInput());
			index = addPage(propertiesEditor, getEditorInput());
		} catch (Exception e) {
			ModelUIPlugin.log(e);
			return;
		}
//		Control control = propertiesEditor.createControl(getContainer());
//		int index = addPage(control);
		setPageText(index, "Properties"); 
		propertiesEditor.setObject(object);
		propertiesEditor.update();
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
		propertiesEditor.setObject(object);
		propertiesEditor.update();
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
		if(propertiesEditor != null) selectionProvider.addHost("guiEditor", propertiesEditor.getSelectionProvider());
		if(textEditor != null) selectionProvider.addHost("textEditor", getTextSelectionProvider());
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
	
}
