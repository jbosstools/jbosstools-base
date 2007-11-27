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
package org.jboss.tools.common.meta.ui.attribute.editor;

import org.jboss.tools.common.model.ui.attribute.editor.*;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class PropertiesStringEditor extends ValueEditor {
	
	public PropertiesStringEditor() {}
	
	public PropertiesStringEditor(IWidgetSettings settings) {
		super(settings);
	}

	public boolean isGreedyEditor() {
		return true;
	}
	
	protected CellEditor createCellEditor(Composite parent) {
		DialogCellEditorEx cellEditor = new DialogCellEditorEx(parent, SWT.NONE);
		cellEditor.setPropertyEditor(this);
		return cellEditor;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		PropertiesStringFieldEditor fieldEditor = new PropertiesStringFieldEditor(settings);
		fieldEditor.setLabelText(getLabelText());
		fieldEditor.setPropertyEditor(this);
		return fieldEditor;
	}

}
