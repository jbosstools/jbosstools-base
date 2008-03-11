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

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class UneditableFieldEditor extends StringFieldEditorEx {
	
	public UneditableFieldEditor() {}
	
	public UneditableFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	public Control createTextControl(Composite parent) {
		Control control = super.createTextControl(parent);
		Text text = getTextField();
		text.setEditable(false);
		return control;
	}

	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
	}

	protected void setTextControlEnabled(boolean enabled) {
	}
	
}
