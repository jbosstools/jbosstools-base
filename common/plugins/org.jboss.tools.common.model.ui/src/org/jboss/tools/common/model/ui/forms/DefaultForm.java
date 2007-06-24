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
package org.jboss.tools.common.model.ui.forms;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class DefaultForm extends AbstractForm {
	
	protected Composite composite;
	protected Control formControl;
	protected Object model;
	
	public DefaultForm() {}

	public Control createControl(Composite parent, IWidgetSettings settings) {
		composite = new Composite(parent, settings.getStyle("Composite.Style"));
		composite.setLayout(getLayout());
		composite.setLayoutData(getLayoutData());
		// create client area
		Control form = createClientArea(composite, settings);
		form.setLayoutData(new GridData(GridData.FILL_BOTH));
		// return
		return composite;
	}

	public Control getControl() {
		return composite;
	}

	// override this method for create form controls	
	protected Control  createClientArea(Composite parent, IWidgetSettings settings) {
		return new Composite(parent, SWT.NONE);
	}

	public void setFocus() {
		this.formControl.setFocus();
	}

	public void dispose() {
		if (this.formControl!=null) this.formControl.dispose();
		this.formControl = null;
		if (this.composite!=null) this.composite.dispose();
		this.composite = null;
	}

	public void initialize(Object model) {
		this.model = model;
	}

	public void commitChanges(boolean onSave) {
	}

	public boolean doGlobalAction(String actionId) {
		return false;
	}

	public void expandTo(Object object) {
	}

	public void update() {
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IForm#store(org.eclipse.ui.IMemento)
	 */
	public void store(IMemento memento) {
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IForm#load(org.eclipse.ui.IMemento)
	 */
	public void load(IMemento memento) {
	}
}
