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
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class StringEditor extends ValueEditor {
	
	public StringEditor() {}
	
	public StringEditor(IWidgetSettings settings) {
		super(settings);
		lineEditor = true;
	}

	protected CellEditor createCellEditor(Composite parent) {
		return (cellEditor = new TextCellEditor(parent, SWT.NONE));
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		return (fieldEditor = new StringFieldEditorEx(settings));
		
	}
}
