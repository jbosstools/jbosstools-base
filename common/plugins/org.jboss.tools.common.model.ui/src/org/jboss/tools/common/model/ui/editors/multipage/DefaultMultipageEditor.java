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
package org.jboss.tools.common.model.ui.editors.multipage;

import org.jboss.tools.common.model.ui.texteditors.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.editor.*;


public class DefaultMultipageEditor extends ObjectMultiPageEditor {
	
	public DefaultMultipageEditor() {}

	protected XMLTextEditorComponent createTextEditorComponent() {
		return new XMLTextEditorComponent();
	}

	protected void doCreatePages() {
		createTreePage();
		createTextPage();
		initEditors();
	}	
	
	protected ObjectTextEditor createTextEditor() {
		return createTextEditorComponent();	
	}

	public boolean isDirty() {
		XModelObject o = getModelObject();
		if((o != null && o.isModified()) || 
		   (textEditor != null && textEditor.isModified())) return true;
		return false;
	}
	
	protected void setNormalMode() {
		if (treeFormPage!=null) { // AU added
			treeFormPage.initialize(getModelObject()); // AU added
			treeFormPage.setErrorMode(isErrorMode());
		} // AU added
		if (selectionProvider!=null) {
			updateSelectionProvider();
		}
		if (treeEditor!=null) { 
			treeEditor.setObject(object, isErrorMode());
		}
	}

	protected void setErrorMode() {
		if (treeFormPage!=null) { // AU added
			treeFormPage.initialize(getModelObject()); // AU added
			treeFormPage.setErrorMode(isErrorMode());
		} // AU added
		if (treeEditor!=null) { 
			treeEditor.setObject(object, isErrorMode());
		}
	}

	protected void checkErrorMode() {
		if(object == null) return;
		boolean i = "yes".equals(object.get("isIncorrect"));
		if(isErrorMode == i) return;
		isErrorMode = i;
/*		
		if(i) {
			setPageText(1, "Errors");
		} else {
			setPageText(1, "Tree");
		}
*/
	}
	
	public void activateErrorTab() {
		if(getPageCount() > 1) setActivePage(1);
	}
	
	protected void updateSelectionProvider() {
		if(textEditor != null) selectionProvider.addHost("textEditor", getTextSelectionProvider());
		if(treeFormPage != null) selectionProvider.addHost("treeEditor", treeFormPage.getSelectionProvider());
		int index = getActivePage();
		if(index == getSourcePageIndex()) {
			if(textEditor != null) {
				selectionProvider.setHost(getTextSelectionProvider());
			}
			return;
		} else {
			if (treeFormPage != null) {
				selectionProvider.setHost(treeFormPage.getSelectionProvider());
			}
		}
	}
	
}
