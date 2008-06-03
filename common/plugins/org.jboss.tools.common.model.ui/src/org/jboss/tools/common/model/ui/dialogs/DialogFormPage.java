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
package org.jboss.tools.common.model.ui.dialogs;

import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.forms.IForm;
import org.jboss.tools.common.model.ui.widgets.DefaultSettings;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * @author Aleksey
 */
public class DialogFormPage extends DialogPage {

	private IForm form;
	private IWidgetSettings settings = new DefaultSettings();
	
	public DialogFormPage() {
		super();
	}

	public void createControl(Composite parent) {
		if (form!=null) {
			setControl(form.createControl(parent, settings));
		}
	}

	public void setForm(IForm form) {
		this.form = form;
	}
}
