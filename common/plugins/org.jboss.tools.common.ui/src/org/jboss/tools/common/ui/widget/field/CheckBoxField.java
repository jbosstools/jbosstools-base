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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public class CheckBoxField extends BaseField implements SelectionListener {
	private Button checkBox = null;
	
	public CheckBoxField(Composite parent) {
		checkBox = new Button(parent, SWT.CHECK);
		checkBox.addSelectionListener(this);
	}

	public void widgetDefaultSelected(SelectionEvent e) {
		checkBox.getSelection();
	}

	public void widgetSelected(SelectionEvent e) {
		firePropertyChange(!checkBox.getSelection(),
				checkBox.getSelection());
	}
	
	public Button getCheckBox() {
		return checkBox;
	}

	@Override
	public Control getControl() {
		return getCheckBox();
	}
	
	
}
