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
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class PropertyEditorEx extends PropertyEditor {

	protected PropertyEditor propertyEditor;
	protected StringButtonFieldEditorEx fieldEditor;
	
	public PropertyEditorEx() {}
	
	public PropertyEditorEx(IWidgetSettings settings) {
		super(settings);
	}
	
	public PropertyEditorEx(PropertyEditor propertyEditor, IWidgetSettings settings) {
		super(settings);
		this.propertyEditor = propertyEditor;
	}

	public void dispose() {
		super.dispose();
		if (propertyEditor!=null) propertyEditor.dispose();
		propertyEditor = null;
		if (fieldEditor!=null) fieldEditor.dispose();
		fieldEditor = null;
	}

	protected CellEditor createCellEditor(Composite parent) {
		return propertyEditor.createCellEditor(parent);
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new StringButtonFieldEditorEx(settings);
		fieldEditor.setLabelText(getLabelText());
		fieldEditor.setPropertyEditor(propertyEditor);
		return fieldEditor;
	}

	public PropertyEditor getPropertyEditor() {
		return propertyEditor;
	}
	
	//
	public Object getValue() {
		return propertyEditor.getValue();
	}
	public void setValue(Object value) {
		propertyEditor.setValue(value);
	}

	//
	public void setLabelText(String labelText) {
		propertyEditor.setLabelText(labelText);
	}
	public String getLabelText() {
		return propertyEditor.getLabelText();
	}


}
