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
package org.jboss.tools.common.resref.core;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.resources.IFile;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintL;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.jst.web.project.WebProject;
import org.jboss.tools.jst.web.tld.TaglibMapping;

public class VpeAddReferenceSupport extends SpecialWizardSupport {

	public static boolean add(IFile file, ResourceReference css, ResourceReference[] list, String entity) {
		return run(file, css, list, "CreateActions.AddItem", entity); //$NON-NLS-1$
	}
	
	public static boolean edit(IFile file, ResourceReference css, ResourceReference[] list, String entity) {
		return run(file, css, list, "EditActions.EditItem", entity); //$NON-NLS-1$
	}
	
	private static boolean run(IFile file, ResourceReference css, ResourceReference[] list, String action, String entity) {
		XModel model = PreferenceModelUtilities.getPreferenceModel();
		XModelObject object = model.createModelObject(entity, null);
		object.setAttributeValue("location", css.getLocation()); //$NON-NLS-1$
		if(object.getAttributeValue("prefix") != null) { //$NON-NLS-1$
			object.setAttributeValue("prefix", css.getProperties()); //$NON-NLS-1$
		}
		Properties p = new Properties();
		p.put("scope",Integer.valueOf(css.getScope())); //$NON-NLS-1$
		p.put("list", list); //$NON-NLS-1$
		if(file != null) p.put("file", file); //$NON-NLS-1$
		XActionInvoker.invoke(action, object, p);
		boolean ok = "true".equals(p.getProperty("okPressed")); //$NON-NLS-1$ //$NON-NLS-2$
		if(ok) {
			css.setLocation(object.getAttributeValue("location")); //$NON-NLS-1$
			Integer scope = (Integer)p.get("scope"); //$NON-NLS-1$
			
			css.setScope(scope.intValue());
			if(css.isGlobal()){
			    css.setScope(ResourceReference.GLOBAL_SCOPE);
			}
			String properties = object.getAttributeValue("prefix"); //$NON-NLS-1$
			if(properties != null) css.setProperties(properties);
		}
		return ok;
	}
	
	IFile file = null;
	String initialLocation;
	String initialPrefix;
	ResourceReference[] list;	
	String[] scopeNames;
	
	protected void reset() {
		initialLocation = getTarget().getAttributeValue("location"); //$NON-NLS-1$
		setAttributeValue(0, "location", initialLocation); //$NON-NLS-1$
		initialPrefix = getTarget().getAttributeValue("prefix"); //$NON-NLS-1$
		if(initialPrefix != null) {
			setAttributeValue(0, "prefix", initialPrefix); //$NON-NLS-1$
		}
        final XAttributeConstraintL scopeAttribute = ((XAttributeConstraintL) getTarget().getModelEntity().getAttribute("scope") //$NON-NLS-1$
                .getConstraint());
        if (scopeAttribute != null) {
            scopeNames = scopeAttribute.getValues();
        }
		int scopeIndex = ((Integer)getProperties().get("scope")).intValue(); //$NON-NLS-1$
		
		if(scopeIndex == 1 && scopeNames.length == 1){
		    scopeIndex = 0;
		}else if(scopeIndex > scopeNames.length){
		    scopeIndex = scopeNames.length -1;
		}
		String scope = scopeNames[scopeIndex];
		setAttributeValue(0, "scope", scope); //$NON-NLS-1$
		list = (ResourceReference[])getProperties().get("list"); //$NON-NLS-1$
		file = (IFile)getProperties().get("file"); //$NON-NLS-1$
		setURIList();
	}
	
	void setURIList() {
		if(file == null) return;
		if(getEntityData()[0].getModelEntity().getName().startsWith("VPETLD")) { //$NON-NLS-1$
			Set set = new TreeSet();
			IModelNature n = EclipseResourceUtil.getModelNature(file.getProject());
			if(n == null) return;
			XModel model = n.getModel();
			TaglibMapping taglibs = WebProject.getInstance(model).getTaglibMapping();
			Map map = taglibs.getTaglibObjects();
			Iterator it = map.keySet().iterator();
			while(it.hasNext()) {
				String s = it.next().toString();
				set.add(taglibs.resolveURI(s));
			}
			String[] uris = (String[])set.toArray(new String[0]);
 			setValueList(0, "location", uris); //$NON-NLS-1$
		}
	}

	public void action(String name) throws XModelException {
		if(OK.equals(name) || FINISH.equals(name)) {
			execute();
			setFinished(true);
			getProperties().setProperty("okPressed", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		} else if(CANCEL.equals(name)) {
			setFinished(true);
		}
	}
	
	protected void execute() throws XModelException {
		Properties p0 = extractStepData(0);
		getTarget().setAttributeValue("location", p0.getProperty("location")); //$NON-NLS-1$ //$NON-NLS-2$
		if(p0.containsKey("prefix")) { //$NON-NLS-1$
			getTarget().setAttributeValue("prefix", p0.getProperty("prefix")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		int scope = getSelectedScope(p0); 
		getProperties().put("scope", Integer.valueOf(scope)); //$NON-NLS-1$
	}
	
	int getSelectedScope(Properties p0) {
		String scopeName = p0.getProperty("scope"); //$NON-NLS-1$
		for (int i = 0; i < scopeNames.length; i++) {
			if(scopeNames[i].equals(scopeName)) return i;
		}
		return 0;
	}

}
