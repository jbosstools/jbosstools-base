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
package org.jboss.tools.common.java.model.handler;

import java.util.Properties;

import org.eclipse.jdt.core.*;

import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.SpecialWizardFactory;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;

public class CreateJavaFileHandler extends AbstractHandler {
	public boolean isEnabled(XModelObject object) {
		if(object == null || !object.isActive()) return false;
		return (object.getAdapter(IJavaElement.class) != null);
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.wizard.newfile.NewClassCreationWizard");
		wizard.setObject(object);
		wizard.execute();
	}
	
}
