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
package org.jboss.tools.common.meta.action.impl;

import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.CreateFileHandler;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class DefaultWizardDataValidator implements WizardDataValidator {
	protected SpecialWizardSupport support;
	protected int step;
	protected String message = null;
	protected String warning = null;
	
	public void setSupport(SpecialWizardSupport support, int step) {
		this.support = support;
		this.step = step;
	}
	
	public int getId() {
		return step;
	}
	
	public void validate(Properties data) {
		message = null;
		warning = null;
		XEntityData[] ds = support.getEntityData();
		if(ds.length <= step) return; 
		if(support.action != null) {
			if("true".equals(support.action.getProperty("validator.add"))) {
				String entity = support.action.getProperty("entity");
				if(entity == null) entity = ds[step].getModelEntity().getName();
				if(!checkChild(support.getTarget(), entity, data)) return;
			} else if("true".equals(support.action.getProperty("validator.edit"))) {
				String entity = support.action.getProperty("entity");
				if(entity == null) entity = ds[step].getModelEntity().getName();
				if(!checkChild(support.getTarget().getParent(), entity, data)) return;
			}
		}
		if(message != null) return;
		XAttributeData[] as = ds[step].getAttributeData();
		for (int i = 0; i < as.length; i++) {
			String n = as[i].getAttribute().getName();
			String value = data.getProperty(n);
			if(value == null) value = "";
			if(!support.isFieldEditorEnabled(step, n, data)) continue;
			message = DefaultCreateHandler.validateAttribute(as[i], value);
			if(message != null) return;
		}
		if(message != null || support.action == null) return;
		if("true".equals(support.action.getProperty("validator.addfile")) && step == 0) {
			validateAddFile(ds, data);
		}
		String resourceAttr = support.action.getProperty("validator.resource");
		if(resourceAttr != null) {
			String value = data.getProperty(resourceAttr);
			if(value != null && (!new java.io.File(value).exists())) { 
				message = "Resource " + value + " does not exist.";
			}
		}
	}
	
	protected void validateAddFile(XEntityData[] ds, Properties data) {
		CreateFileHandler.validateNameAndExtension(support.action, data, null);
		String entity = support.action.getProperty("entity");
		if(entity == null) {
			String ext = null;
			entity = (ext != null) ? support.getTarget().getModel().getEntityRecognizer().getEntityName(ext, null)
						: ds[step].getModelEntity().getName();
			if(entity == null || support.getTarget().getModel().getMetaData().getEntity(entity) == null)
			  entity = ds[step].getModelEntity().getName();
		}
		try {
			XModelObject o = support.getTarget().getModel().createModelObject(entity, data);
			if(o != null) message = DefaultCreateHandler.getContainsMessage(support.getTarget(), o);
		} catch (Exception e) {
			ModelPlugin.log("DefaultWizardDataValidator:validateAddFile:" + e.getMessage());
		}
	}
	
	public String getErrorMessage() {
		return message;
	}
	
	public String getWarningMessage() {
		return warning;
	}

	public boolean isCommandEnabled(String command) {
		if(message != null) { 
			if(SpecialWizardSupport.OK.equals(command) || SpecialWizardSupport.NEXT.equals(command) || SpecialWizardSupport.FINISH.equals(command)) return false;
		}
		return support.isActionEnabled(command);
	}
	
	protected boolean checkChild(XModelObject parent, String entity, Properties data) {
		try {
			XModelObject o = support.getTarget().getModel().createModelObject(entity, data);
			if(o.getModelEntity().getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null) return true;
			if(o != null && parent == support.getTarget().getParent() && o.getPathPart().equals(support.getTarget().getPathPart())) return true;
			if(o != null) message = DefaultCreateHandler.getContainsMessage(parent, o);
		} catch (Exception e) {
			ModelPlugin.log("DefaultWizardDataValidator:checkChild:" + e.getMessage());
		}
		return message == null;
	}

}
