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
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.forms.IForm;

/**
 * @author Igels
 *
 */
public class FormFactory extends XModelObjectFormFactory {

	private Class formClass;

	public FormFactory(XModelObject xmo) {
		super(xmo);
		initialize();
	}

	private void initialize() {
//		String entityName = getXModelObject().getModelEntity().getName();
		String formClassName = null;
		formClassName = getXModelObject().getModelEntity().getProperty("formClassName");
		formClass = ModelFeatureFactory.getInstance().getFeatureClass(formClassName);
	}

    /**
     * @see org.jboss.tools.common.model.ui.forms.IFormFactory#getForm(java.lang.Object)
     */
    public IForm getForm() {
    	IForm form = null;
		try {
			form = (IForm)formClass.newInstance();
		} catch(Exception e) {
			ModelUIPlugin.log(e);
		}
		return form;
    }
}