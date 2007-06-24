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

/**
 * @author glory
 * 
 * Class DefaultRenameSupport does not completes rename operation. 
 * It changes XEntityData and validates its own copy of XModelObject 
 * modified by the changed data. Class should be used by static run() 
 * method with object to be renamed, consistent XEntityData including 
 * id attributes. If method returns 0, caller may complete rename
 * using changed data. 
 */

public class DefaultRenameSupport extends SpecialWizardSupport {
	
	public static int run(XModelObject object, XEntityData data, Properties p) {
		return run(object, data, p, new DefaultRenameSupport());
	}

	public static int run(XModelObject object, XEntityData data, Properties p, DefaultRenameSupport support) {
		support.setActionData(null, new XEntityData[]{data}, object, p);
		object.getModel().getService().showDialog(support);
		return support.getReturnCode();
	}
	protected int returnCode = -1;
	protected XModelObject source;
	protected XModelObject copy;
	
	public void reset() {
		returnCode = -1;
		source = getTarget();
		copy = source.copy();
	}

	public String getTitle() {
		String title = getProperties().getProperty("title");
		if(title != null) return title;
		return "Rename " + PasteEnterNewNameSupport.getCapitalizedName(getTarget());
	}

	public String getMessage(int stepId) {
		String displayName = WizardKeys.getAttributeDisplayName(getEntityData()[0].getAttributeData()[0].getAttribute(), true);
		return "Please enter new " + displayName + ".";
	}

	public String[] getActionNames(int stepId) {
		return new String[]{FINISH, CANCEL};
	}

	public void action(String name) throws Exception {
		if(name.equals(FINISH)) {
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
	
	RenameValidator pasteValidator = new RenameValidator();

	public WizardDataValidator getValidator(int step) {
		pasteValidator.setSupport(this, step);
		return pasteValidator;    	
	}
	
	class RenameValidator extends DefaultWizardDataValidator {
		public void validate(Properties data) {
			super.validate(data);
			if(message != null) return;
			applyDataToCopy(data, step);
			if(source.getPathPart().equals(copy.getPathPart())) {
				message = support.getMessage(step);
			} else {
				message = DefaultCreateHandler.getContainsMessage(source.getParent(), copy);
			}
		}
	}
	
	protected void applyDataToCopy(Properties data, int step) {
		XAttributeData[] ad = getEntityData()[step].getAttributeData();
		for (int i = 0; i < ad.length; i++) {
			String n = ad[i].getAttribute().getName();
			String v = data.getProperty(n);
			if(v != null) copy.setAttributeValue(n, v);				
		}
	}

}
