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

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.forms.DefaultFormContainer;
import org.jboss.tools.common.model.ui.forms.IForm;

/**
 * @author Igels
 */
public class FormContainer  extends DefaultFormContainer {

	private IFormData formData;
	
	private FormContainer() {}

	public FormContainer(IFormData formData) {
		super();
		this.formData = formData;
		this.setHeadingText(formData.getEntityName());

		IForm form = null;
		IFormData[] forms = formData.getForms();
//		String[] entities = formData.getEntities();
		for(int i=0; i<forms.length; i++) {
			if(!forms[i].isNotLayouredForm()) {
				form = new Form(forms[i]);
			} else {
				try {
					form = (IForm)ModelFeatureFactory.getInstance().createFeatureInstance(forms[i].getFormClassName());
				} catch(Exception e) {
					ModelUIPlugin.log(e);
				}
			}
			if(form!=null) {
				this.addForm(form);
				form.setParent(this);
			}
		}
	}

	public void initialize(Object model) {
		IFormData[] forms = formData.getForms();
		String[] entities = formData.getEntities();
		if (forms!=null && entities!=null) {
			XModelObject xmo = (XModelObject)model;
			XModelObject child;
			for (int i=0;i<forms.length;++i) {
				if(forms[i].getEntityName()==null) {
					this.get(i).initialize(xmo);
					continue;
				}
				String entity = forms[i].getEntityName();
				// Here it is not entity but rather a unique child name
				child = xmo.getChildByPath(entity);
				if(child == null) {
					String message = "Cannot build child form '" + forms[i].getEntityName() + "' for form '" + formData.getEntityName() + "'.";
					Exception exc = new RuntimeException(message);
					ProblemReportingHelper.reportProblem("org.jboss.tools.common.model.ui", exc);
				} else {
					this.get(i).initialize(child);
				}
			}
		} else {
			super.initialize(model);
		}
	}
}