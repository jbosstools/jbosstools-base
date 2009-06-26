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
package org.jboss.tools.common.model.ui.templates.preferences;

import java.util.Properties;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.model.*;

public class AddInterfaceSupport extends SpecialWizardSupport {
	
	public static String runAdd(XModel model) {
		return runAdd(model, "CreateActions.AddInterface", "interface"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static String runAddClass(XModel model) {
		return runAdd(model, "CreateActions.AddClass", "class"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static String runAdd(XModel model, String action, String property) {
		String entity = "TemplateClass"; //$NON-NLS-1$
		Properties p = new Properties();
		XModelObject o = model.createModelObject(entity, p);
		XActionInvoker.invoke(action, o, p);
		return p.getProperty(property);
	}

	public static String runEdit(XModel model, String current) {
		String entity = "TemplateClass"; //$NON-NLS-1$
		Properties p = new Properties();
		if(current != null) p.setProperty("interface", current); //$NON-NLS-1$
		XModelObject o = model.createModelObject(entity, p);
		XActionInvoker.invoke("EditActions.EditInterface", o, p); //$NON-NLS-1$
		return p.getProperty("interface"); //$NON-NLS-1$
		
	}

	String property;

	public void reset() {
		property = getEntityData()[0].getAttributeData()[0].getAttribute().getName();
		if(property == null) property = "interface"; //$NON-NLS-1$
		String current = getProperties().getProperty(property);
		if(current != null) setAttributeValue(0, property, current);		
	}

	public void action(String name) throws XModelException {
		if(OK.equals(name) || FINISH.equals(name)) {
			getProperties().setProperty(property, getAttributeValue(0, property));
			setFinished(true);
		} else if(CANCEL.equals(name)) {
			getProperties().remove(property);
			setFinished(true);			
		}
	}

}
