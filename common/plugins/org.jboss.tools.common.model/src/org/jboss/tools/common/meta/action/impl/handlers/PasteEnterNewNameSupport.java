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
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.*;

public class PasteEnterNewNameSupport extends SpecialWizardSupport {
	
	public static int run(XModelObject parent, XModelObject source, XModelObject copy, XEntityData data) {
		PasteEnterNewNameSupport support = new PasteEnterNewNameSupport();
		Properties p = new Properties();
		p.put("source", source);
		p.put("copy", copy);
		support.setActionData(null, new XEntityData[]{data}, parent, p);
		parent.getModel().getService().showDialog(support);
		return support.getReturnCode();
	}
	int returnCode = -1;
	XModelObject source;
	XModelObject copy;
	
	public void reset() {
		returnCode = -1;
		source = (XModelObject)getProperties().get("source");
		copy = (XModelObject)getProperties().get("copy");
	}

	public String getTitle() {
		return "Paste " + getCapitalizedName(copy) + " to "
			   + getCapitalizedName(getTarget());
	}
	
	static String getCapitalizedName(XModelObject o) {
		String n = o.getAttributeValue("element type");
		if(n == null || n.length() == 0) n = o.getModelEntity().getXMLSubPath();
		if(n == null || n.length() == 0) n = o.getPathPart();
		return WizardKeys.toDisplayName(n);
	}

	public String getMessage(int stepId) {
		String displayName = WizardKeys.getAttributeDisplayName(getEntityData()[0].getAttributeData()[0].getAttribute(), true);
		return "Please enter new " + displayName + ".";
	}

	public String[] getActionNames(int stepId) {
		return new String[]{OK, CANCEL/*, HELP*/};
	}

	public void action(String name) throws Exception {
		if(name.equals(OK)) {
			returnCode = 0;
			setFinished(true);
		} else if(name.equals(CANCEL)) {
			returnCode = 1;
			setFinished(true);
		}
	}
	
	public int getReturnCode() {
		return returnCode;
	}
	
	PasteValidator pasteValidator = new PasteValidator();

	public WizardDataValidator getValidator(int step) {
		pasteValidator.setSupport(this, step);
		return pasteValidator;    	
	}
	
	class PasteValidator extends DefaultWizardDataValidator {
		public void validate(Properties data) {
			super.validate(data);
			if(message != null) return;
			XAttributeData[] ad = getEntityData()[step].getAttributeData();
			for (int i = 0; i < ad.length; i++) {
				String n = ad[i].getAttribute().getName();
				String v = data.getProperty(n);
				if(v != null) {
					if(ad[i].getAttribute().isTrimmable()) v = v.trim();
					copy.setAttributeValue(n, v);				
				}
			}
			if(source.getPathPart().equals(copy.getPathPart())) {
				message = support.getMessage(step);
			} else {
				message = DefaultCreateHandler.getContainsMessage(support.getTarget(), copy);
			}
		}
	}

}
