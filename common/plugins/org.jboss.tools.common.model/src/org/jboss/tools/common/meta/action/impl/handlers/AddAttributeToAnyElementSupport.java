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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.AnyElementObjectImpl;

public class AddAttributeToAnyElementSupport extends SpecialWizardSupport {
	Set<String> attributes = new HashSet<String>();
	boolean edit = false;
	String initialName = null;
	String initialValue = null;

	public boolean isEnabled(XModelObject object) {
		return (object != null && object.isObjectEditable());
	}	

	public void reset() {
		attributes.clear();
		String[][] as = ((AnyElementObjectImpl)getTarget()).getAttributes();
		for (int i = 0; i < as.length; i++) {
			attributes.add(as[i][0]);
		}
		edit = "true".equals(action.getProperty("edit"));
		initialName = getProperties().getProperty("name");
		initialValue = null;
		if(initialName != null) {
			setAttributeValue(0, "name", initialName);
			for (int i = 0; i < as.length; i++) {
				if(initialName.equals(as[i][0])) initialValue = as[i][1];
			}
			if(initialValue != null) setAttributeValue(0, "value", initialValue);
		}
	}

	public void action(String name) throws Exception {
		if(OK.equals(name)) {
			finish();
			setFinished(true);
		} else if(CANCEL.equals(name)) {
			setFinished(true);
		} else if(HELP.equals(name)) {
			help();
		}
	}
	
	protected void finish() throws Exception {
		Properties p = extractStepData(0);
		String name = p.getProperty("name");
		String value = p.getProperty("value");
		String as = "";
		if(!edit) {
			as = getTarget().getAttributeValue("attributes");
			if(as.length() > 0) as += AnyElementObjectImpl.SEPARATOR;
			as += name + "=" + value;
		} else {
			String[][] attrs = ((AnyElementObjectImpl)getTarget()).getAttributes();
			StringBuffer sb = new StringBuffer();
			for (int i = 0; i < attrs.length; i++) {
				if(sb.length() > 0) sb.append(AnyElementObjectImpl.SEPARATOR);
				sb.append(attrs[i][0]).append('=');
				if(attrs[i][0].equals(name)) sb.append(value); else sb.append(attrs[i][1]);
			}
			as = sb.toString();
		}
		getTarget().getModel().editObjectAttribute(getTarget(), "attributes", as);
	}

    protected DefaultWizardDataValidator validator = new Validator();
    
    public WizardDataValidator getValidator(int step) {
    	validator.setSupport(this, step);
		return validator;    	
    }

    class Validator extends DefaultWizardDataValidator {
    	public void validate(Properties data) {
    		super.validate(data);
    		if(message != null) return;
    		String name = data.getProperty("name");    		
    		if(!edit && attributes.contains(name)) {
    			message = "Attribute " + name + " already exists.";
    		}
    		if(message != null) return;
    		if(edit && initialValue != null && name.equals(initialName)) {
    			if(initialValue.equals(data.getProperty("value"))) {
    				message = "Please set new value.";
    			}
    		}
    	}
    }

    public boolean isFieldEditorEnabled(int stepId, String name, Properties values) {
    	return !edit || !name.equals("name");
    }
    
}
