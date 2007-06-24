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
import org.jboss.tools.common.model.ui.forms.IForm;

/**
 * @author Igels
 */
public class LayouredFormFactory extends XModelObjectFormFactory {

	private IFormLayoutData formLayoutData;

	public LayouredFormFactory(XModelObject xmo) {
		super(xmo);
		initialize();
	}

	private void initialize() {
//		String entityName = getXModelObject().getModelEntity().getName();
		String formLayoutDataClassName = null;
		formLayoutDataClassName = getXModelObject().getModelEntity().getProperty("formLayoutDataClassName");
		Class formLayoutDataClass = ModelFeatureFactory.getInstance().getFeatureClass(formLayoutDataClassName);
		try {
			formLayoutData = (IFormLayoutData)formLayoutDataClass.getMethod("getInstance", (Class[])null).invoke(null, (Object[])null);
		} catch(Exception e) {
			ModelUIPlugin.log(e);
		}
	}

	// IFormFactory

	public IForm getForm() {
		IForm form = null;
		try {
			String entity = getXModelObject().getModelEntity().getName();
			IFormData formData = formLayoutData.getFormData(entity);
			if(formData == null) {
				String message = "Cannot find form for entity " + entity + ".";
				ProblemReportingHelper.reportProblem("org.jboss.tools.common.model.ui", new Exception(message));
			} else if(formData.getForms() != null) {
				form = new FormContainer(formData);
			} else {
				form = new Form(formData);
			}
		} catch(Exception e) {
			String message = "Cannot build form.";
			ProblemReportingHelper.reportProblem("org.jboss.tools.common.model.ui", message, e);
		}

		return form;
	}
}