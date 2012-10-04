/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui.wizard.service;

import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IType;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISources;
import org.eclipse.ui.handlers.HandlerUtil;
import org.jboss.tools.common.java.IParametedType;
import org.jboss.tools.common.java.ParametedType;
import org.jboss.tools.common.java.ParametedTypeFactory;
import org.jboss.tools.common.ui.CommonUIPlugin;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class RegisterAsServiceHandler extends AbstractHandler {

	public RegisterAsServiceHandler() {
	}

	@Override
	public void setEnabled(Object evaluationContext) {
		setBaseEnabled(computeEnabled(evaluationContext));
	}

	private boolean computeEnabled(Object evaluationContext) {
		if(evaluationContext instanceof IEvaluationContext) {
			IEvaluationContext c = (IEvaluationContext)evaluationContext;
			ISelection selection = (ISelection)c.getVariable(ISources.ACTIVE_CURRENT_SELECTION_NAME);
			return getSelectedType(selection) != null;
		}
		return false;
	}

	/**
	 * Returns the first IType from selection which is a concrete class.
	 * @param selection
	 * @return
	 */
	private IType getSelectedType(ISelection selection) {
		if(selection != null && !selection.isEmpty() && (selection instanceof IStructuredSelection)) {
			for (Object selected: ((IStructuredSelection)selection).toList()) {
				try {
					if(selected instanceof ICompilationUnit) {
						ICompilationUnit u = (ICompilationUnit)selected;
						for(IType type: u.getTypes()) {
							if(accept(type)) {
								return type;
							}
						}
					} else if(selected instanceof IType) {
						IType type = (IType)selected;
						if(accept(type)) {
							return type;
						}
					}
				} catch (CoreException e) {
					CommonUIPlugin.getDefault().logError(e);
				}
			}
		}
		return null;
	}

	private boolean accept(IType type) throws CoreException {
		return !type.isInterface() && !type.isAnnotation() && !Flags.isAbstract(type.getFlags());
	}

	@Override
	public Object execute(ExecutionEvent event) throws org.eclipse.core.commands.ExecutionException {
	    ISelection selection = HandlerUtil.getActiveWorkbenchWindow(event).getSelectionService().getSelection();
	    IType type = getSelectedType(selection);
    	if(type == null) {
    		return null;
    	}
	    try {
	    	ParametedType parametedType = new ParametedTypeFactory().newParametedType(type);
	    	Collection<IParametedType> ts = parametedType.getAllTypes();
	    	Map<String, IParametedType> types = new TreeMap<String, IParametedType>();
	    	for (IParametedType t: ts) {
	    		if(t.getType() != null) {
	    			String q = t.getType().getFullyQualifiedName();
	    			types.put(q, t);
	    		}
	    	}
	    	types.remove("java.lang.Object"); //$NON-NLS-1$
	    	types.remove(type.getFullyQualifiedName());
	    	
	    	RegisterAsServiceDialog dialog = new RegisterAsServiceDialog(HandlerUtil.getActiveShell(event), type, types);
	    	dialog.create();
	    	
	    	int i = dialog.open();
	    	if(i == IDialogConstants.OK_ID) {
	    		IProject project = type.getJavaProject().getProject();
	    		String typeName = type.getFullyQualifiedName();
	    		String serviceType = dialog.getResult();
	    		RegisterServiceUtil.registerService(project, typeName, serviceType);
	    	}
	    } catch (CoreException e) {
	    	CommonUIPlugin.getDefault().logError(e);
	    }
		return null;
	}

}
