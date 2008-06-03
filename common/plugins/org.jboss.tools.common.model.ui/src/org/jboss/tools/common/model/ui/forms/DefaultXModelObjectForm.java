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

import org.jboss.tools.common.editor.form.SamplePropertyForm;

import org.jboss.tools.common.model.ui.forms.DefaultFormContainer;

/**
 * @author Aleksey
 */
public class DefaultXModelObjectForm extends DefaultFormContainer {

	private SamplePropertyForm propertyEditorForm;
	//private IForm childEditorForm;

	public DefaultXModelObjectForm() {
		super();
		// property form
		propertyEditorForm = new SamplePropertyForm();
		this.addForm(propertyEditorForm);
		propertyEditorForm.setParent(this);
		propertyEditorForm.setCollapsable(Boolean.FALSE.booleanValue());

		// child editor form
		//childEditorForm = new SampleChildForm();
		//this.addForm(childEditorForm);
		//childEditorForm.setParent(this);

		this.setHeadingText("");
	}
}