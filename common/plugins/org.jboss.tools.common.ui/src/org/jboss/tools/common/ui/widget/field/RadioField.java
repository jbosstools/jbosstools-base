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

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * A group of labels and radio buttons.
 * @author Alexey Kazakov
 */
public class RadioField extends BaseField implements SelectionListener {

	private Composite topComposite;
	private Button[] radios;
	private Object value;

	public RadioField(Composite parent,  List<String> labels, List<Object> values, Object defaultValue, boolean verticalLayout) {
		topComposite = new Composite(parent, SWT.NONE);
		topComposite.setLayout(new GridLayout(verticalLayout ? 1 : values.size(), false));
		if(verticalLayout) {
			topComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}

		radios = new Button[values.size()];
		if(defaultValue==null && !values.isEmpty()) {
			defaultValue = values.get(0);
		}
		for (int i = 0; i < radios.length; i++) {
			radios[i] = new Button(topComposite, SWT.RADIO);
			radios[i].setText(labels.get(i));
			radios[i].addSelectionListener(this);
			radios[i].setLayoutData(new GridData(verticalLayout ? GridData.FILL_HORIZONTAL : GridData.BEGINNING));
			Object value = values.get(i);
			radios[i].setData(value);
			if(value != null && value.equals(defaultValue)) {
				radios[i].setSelection(true);
				this.value = value;
			}
		} 
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.field.BaseField#getControl()
	 */
	@Override
	public Control getControl() {
		return topComposite;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	public void widgetDefaultSelected(SelectionEvent e) {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	public void widgetSelected(SelectionEvent event) {
		value = event.widget.getData();
		firePropertyChange("", value); //$NON-NLS-1$
	}

	/**
	 * @return Value of selected button
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * 
	 * @param value
	 */
	public void setValue(String value) {
		this.value = value;
		for (int i = 0; i < radios.length; i++) {
			if(value.equals(radios[i].getData())) {
				radios[i].setSelection(true);
			} else {
				radios[i].setSelection(false);
			}
		}
	}
}