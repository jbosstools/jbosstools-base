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
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class RadioArrayEditor extends ComboBoxEditor {
	protected RadioArrayFieldEditor fieldEditor;

	public RadioArrayEditor() {}

	public RadioArrayEditor(IWidgetSettings settings) {
		super(settings);
	}

	public void dispose() {
		super.dispose();
		if (fieldEditor!=null) fieldEditor.dispose();
		fieldEditor = null;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new RadioArrayFieldEditor(settings);
		fieldEditor.setLabelText(getLabelText());
		return fieldEditor;
	}

}
