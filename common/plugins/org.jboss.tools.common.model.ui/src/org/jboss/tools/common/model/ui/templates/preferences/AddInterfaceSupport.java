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
		return runAdd(model, "CreateActions.AddInterface", "interface");
	}

	public static String runAddClass(XModel model) {
		return runAdd(model, "CreateActions.AddClass", "class");
	}

	private static String runAdd(XModel model, String action, String property) {
		String entity = "TemplateClass";
		Properties p = new Properties();
		XModelObject o = model.createModelObject(entity, p);
		XActionInvoker.invoke(action, o, p);
		return p.getProperty(property);
	}

	public static String runEdit(XModel model, String current) {
		String entity = "TemplateClass";
		Properties p = new Properties();
		if(current != null) p.setProperty("interface", current);
		XModelObject o = model.createModelObject(entity, p);
		XActionInvoker.invoke("EditActions.EditInterface", o, p);
		return p.getProperty("interface");
		
	}

	String property;

	public void reset() {
		property = getEntityData()[0].getAttributeData()[0].getAttribute().getName();
		if(property == null) property = "interface";
		String current = getProperties().getProperty(property);
		if(current != null) setAttributeValue(0, property, current);		
	}

	public void action(String name) throws Exception {
		if(OK.equals(name)) {
			getProperties().setProperty(property, getAttributeValue(0, property));
			setFinished(true);
		} else if(CANCEL.equals(name)) {
			getProperties().remove(property);
			setFinished(true);			
		}
	}

}
