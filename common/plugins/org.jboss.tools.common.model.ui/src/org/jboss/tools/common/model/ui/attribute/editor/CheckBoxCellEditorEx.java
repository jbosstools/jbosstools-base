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

import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public class CheckBoxCellEditorEx extends ComboBoxCellEditorEx {

	protected Combo comboBox;
	protected Composite parent;

	protected String trueValue = "true"; //$NON-NLS-1$
	protected String falseValue = "false"; //$NON-NLS-1$
	
	public CheckBoxCellEditorEx(Composite parent, String[] items, int style) {
		super(parent, items, style);
	}
	
	protected Control createControl(Composite parent) {
		this.comboBox = (Combo)super.createControl(parent);
		return this.comboBox;
	}
	protected void doSetValue(Object value) {
		initValue(value);
		
		if(comboBox != null) {
			comboBox.setItems(getTags());
			comboBox.setText("" + value); //$NON-NLS-1$
		} 
	}
	protected Object doGetValue() {
		return comboBox.getText();
	}

	private String[] getTags() {
		return new String[] {trueValue, falseValue};
	}

	private void initValue(Object value) {
		if (value instanceof String) {
			if ("yes".equalsIgnoreCase(value.toString()) || ("no".equalsIgnoreCase(value.toString()))) { //$NON-NLS-1$ //$NON-NLS-2$
				trueValue = "yes"; //$NON-NLS-1$
				falseValue = "no"; //$NON-NLS-1$
			}
		}
	}
}
