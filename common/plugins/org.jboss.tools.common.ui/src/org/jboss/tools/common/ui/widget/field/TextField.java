/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.field;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

public class TextField extends BaseField implements ModifyListener{


	Text textField = null;
	
	public TextField(Composite parent, int style) {
		textField = new Text(parent,style);
		textField.addModifyListener(this);
	}

	public void modifyText(ModifyEvent e) {
		firePropertyChange(new Object(),((Text)e.widget).getText().trim());
	}

	public Text getTextControl() {
		return textField;
	}

	@Override
	public Control getControl() {
		return getTextControl();
	}
}
