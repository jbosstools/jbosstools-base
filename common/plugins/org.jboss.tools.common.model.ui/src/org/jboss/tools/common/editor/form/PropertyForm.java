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
package org.jboss.tools.common.editor.form;

import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.objecteditor.XModelObjectEditor;

public class PropertyForm implements IForm {
	XModelObjectEditor editor = new XModelObjectEditor();

	public void setInput(Object object) {
		editor.stopEditing();
		editor.setModelObject((XModelObject)object);
	}

	public Control createControl(Composite parent) {
		return editor.createControl(parent);
	}

	public Control getControl() {
		return editor.getControl();
	}

	public void dispose() {
		editor.stopEditing();
		editor.update(); //!
		editor.dispose();
		editor = null;
	}

	public void update() {
		editor.update();
	}
    
}
