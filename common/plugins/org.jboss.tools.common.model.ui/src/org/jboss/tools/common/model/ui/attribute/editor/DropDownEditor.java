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

import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultComboBoxValueAdapter;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class DropDownEditor extends SelectionEditor {

	protected ComboBoxCellEditorEx cellEditor;
	protected ComboBoxFieldEditor fieldEditor;

	public DropDownEditor() {}

	public DropDownEditor(IWidgetSettings settings) {
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
		cellEditor = new ComboBoxCellEditorEx(parent, getTags(), SWT.READ_ONLY | SWT.NONE);
		return cellEditor;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new ComboBoxFieldEditor(settings);
		fieldEditor.setDropDown(Boolean.TRUE.booleanValue());
		fieldEditor.setLabelText(getLabelText());
		fieldEditor.setPropertyEditor(this);
		return fieldEditor;
	}

	private String[] getTags() {
		IListContentProvider listContentProvider = (IListContentProvider)getAdapter(IListContentProvider.class);
		ILabelProvider labelProvider = (ILabelProvider)getAdapter(ILabelProvider.class);
		
		Object[] elements = listContentProvider.getElements(this);
		String[] tags = new String[elements.length];
		for(int i=0;i<elements.length;++i){ 
			tags[i] = labelProvider.getText(elements[i]);
		}
		return tags;
	}

	public Object getInput() {
		if (input==null) {
			setInput(new DefaultComboBoxValueAdapter());
		}
		return input;
	}
}
