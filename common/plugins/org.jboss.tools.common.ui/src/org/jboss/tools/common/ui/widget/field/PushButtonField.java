/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.field;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;

/**
 * @author eskimo
 *
 */
public class PushButtonField extends BaseField {
	
	Button button;
	
	/**
	 * 
	 */
	@Override
	public Control getControl() {
		return button;
	}


	public PushButtonField(Composite composite, ButtonPressedAction listener) {
		button = new Button(composite,SWT.PUSH);
		button.setText(listener.getText());
		button.addSelectionListener(listener);
	}
}
