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

import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.ui.*;

public class ActiveCellEditor extends DialogCellEditorEx {

	public ActiveCellEditor(Composite parent, int style) {
		super(parent, style);
	}

	public Object callExternal(Shell shell) {
		IActionHelper action = (IActionHelper)propertyEditor.getAdapter(IActionHelper.class);
		return action.invoke(getTextControl().getShell());
	}
	
}

