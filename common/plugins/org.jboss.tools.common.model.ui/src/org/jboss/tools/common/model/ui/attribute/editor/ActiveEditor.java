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
package org.jboss.tools.common.model.ui.attribute.editor;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class ActiveEditor extends ValueEditor {

	ActiveCellEditor cellEditor;
	ActiveFieldEditorEx fieldEditor;
	
	public ActiveEditor() {}
	
	public ActiveEditor(IWidgetSettings settings) {
		super(settings);
		lineEditor = true;
	}

	public void dispose() {
		super.dispose();
		if (cellEditor!=null) cellEditor.dispose();
		cellEditor = null;
		if (fieldEditor!=null) fieldEditor.dispose();
		fieldEditor = null;
	}
	
	protected CellEditor createCellEditor(Composite parent) {
		cellEditor = new ActiveCellEditor(parent, SWT.NONE);
		cellEditor.setPropertyEditor(this);
		return cellEditor;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new ActiveFieldEditorEx(settings);
		fieldEditor.setLabelText(getLabelText());
		fieldEditor.setPropertyEditor(this);
		return fieldEditor;
	}
	
	public boolean callsExternal() {
		return true;
	}

	public Object callExternal(Shell shell) {
		return cellEditor.callExternal(shell);
	}
	
}
