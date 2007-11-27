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

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class CheckBoxEditor extends ValueEditor {

	protected CheckBoxCellEditorEx cellEditor; 
	protected CheckBoxFieldEditor fieldEditor;

	public CheckBoxEditor() {}

	public CheckBoxEditor(IWidgetSettings settings) {
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
		cellEditor = new CheckBoxCellEditorEx(parent, new String[0], SWT.READ_ONLY | SWT.NONE);
		return cellEditor;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
//		IValueProvider valueProvider = (IValueProvider)getAdapter(IValueProvider.class);
		fieldEditor = new CheckBoxFieldEditor(settings);
		fieldEditor.setLabelText(getLabelText());
		return fieldEditor;
	}

}
