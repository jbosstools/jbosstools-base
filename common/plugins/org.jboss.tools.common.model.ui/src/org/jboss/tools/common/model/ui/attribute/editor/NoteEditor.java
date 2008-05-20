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

import org.jboss.tools.common.model.ui.IActionHelper;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class NoteEditor extends ValueEditor {
	
	protected DialogCellEditorEx cellEditor;
	protected NoteFieldEditor fieldEditor;
	
	public NoteEditor() {}
	
	public NoteEditor(IWidgetSettings settings) {
		super(settings);
	}

	public void dispose() {
		super.dispose();
		if (cellEditor!=null) cellEditor.dispose();
		cellEditor = null;
		if (fieldEditor!=null) fieldEditor.dispose();
		fieldEditor = null;
	}

	public boolean isGreedyEditor() {
		return true;
	}
	
	protected CellEditor createCellEditor(Composite parent) {
		boolean b = (getActionProvider() != null);
		cellEditor = (b) ? new ActiveCellEditor(parent, SWT.NONE)
		             : new DialogCellEditorEx(parent, SWT.NONE);
		cellEditor.setPropertyEditor(this);
		return cellEditor;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new NoteFieldEditor(settings);
		fieldEditor.setLabelText(getLabelText());
		fieldEditor.setPropertyEditor(this);
		return fieldEditor;
	}
	
	public boolean callsExternal() {
		return getActionProvider() != null;
	}
	
	IActionHelper getActionProvider() {
		return (IActionHelper)getAdapter(IActionHelper.class);
	}

	public Object callExternal(Shell shell) {
		ActiveCellEditor c = (ActiveCellEditor)cellEditor;
		return c.callExternal(shell);
	}
	
}
