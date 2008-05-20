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

import org.jboss.tools.common.model.ui.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class ActiveFieldEditorEx extends StringButtonFieldEditorEx {
	boolean edit_inline = true;
	
	public ActiveFieldEditorEx() {}
	
	public ActiveFieldEditorEx(IWidgetSettings settings) {
		super(settings);
	}
	
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		super.setPropertyEditor(propertyEditor);
		IActionHelper action = (IActionHelper)propertyEditor.getAdapter(IActionHelper.class);
		String command = null;
		if (action!=null) {
			command = action.getCommand();
		}
		if(command != null && !command.equals("...")) {
			setChangeButtonText(command);
		}
		if(action instanceof IActionHelperExtension) {
			edit_inline = ((IActionHelperExtension)action).isEditableInline();
		}
	}

	protected String changePressed() {
		IActionHelper action = (IActionHelper)propertyEditor.getAdapter(IActionHelper.class);
		return action.invoke(composite.getShell());
	}

	protected Control createTextChangeControl(Composite parent) {
		Control c = super.createTextChangeControl(parent);
		getTextField().setEditable(edit_inline);
		return c;
	}
}
