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
package org.jboss.tools.common.model.ui.action;

import java.io.*;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.actions.RefreshAction;

import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public abstract class AddNatureActionDelegate implements IObjectActionDelegate, IWorkbenchWindowActionDelegate {
	protected IProject project;
	
	public AddNatureActionDelegate() {}

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {}

	public void run(IAction action) {
		if (action != null && !action.isEnabled()) return;
		if(project == null) {
			if(action != null) {
				action.setEnabled(false);
			}
			return;
		}
		try	{
			doRun();
		} catch(Exception ex) {
			ProblemReportingHelper.reportProblem(ModelUIPlugin.PLUGIN_ID, ex);
		}
	}
	
	protected void doRun() throws Exception {
		if(project == null) return;
		RefreshAction refreshAction = new RefreshAction(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell());
		refreshAction.selectionChanged(new StructuredSelection(project));
		refreshAction.run();
				
		IWizard wizard = getWizard(project);
		WizardDialog dialog = new WizardDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
		dialog.open();  
	}
	
	protected abstract IWizard getWizard(IProject project) throws Exception;
	protected abstract String getNatureID();

	public void selectionChanged(IAction action, ISelection selection) {
		project = null;
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection structuredSelection = (IStructuredSelection)selection; 
			if (structuredSelection.size() == 1) {
				Object object = structuredSelection.getFirstElement();
				if(object instanceof IResource) {
					project = ((IResource)object).getProject();
				} else if(object instanceof IJavaElement) {
					project = ((IJavaElement)object).getJavaProject().getProject();
				}
					
				if(project != null) 
					try {
						if (!project.isOpen() || project.hasNature(getNatureID()))
							project = null;
					} catch (Exception ex) {
						project = null;
						ModelUIPlugin.log(ex);
					}
			}
		}
		if(action != null) action.setEnabled(project != null);
	}
	
	protected String findWebXML(String root) {
		if(root == null) return "";
		String s = root + "/WEB-INF/web.xml";
		if(new File(s).isFile()) return s;
		File rf = new File(root);
		File[] fs = rf.listFiles();
		if(fs == null) return "";
		for (int i = 0; i < fs.length; i++) { 
			s = fs[i].getAbsolutePath().replace('\\', '/') + "/WEB-INF/web.xml";
			if(new File(s).isFile()) return s;
		}		
		return "";
	}

	public void dispose() {}

	protected boolean isWindowAction;

	public void init(IWorkbenchWindow window) {
		isWindowAction = true;
	}
	
}
