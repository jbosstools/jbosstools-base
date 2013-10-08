/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.internal.handlers;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.actions.RefreshAction;
import org.eclipse.ui.handlers.HandlerUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public abstract class AddNatureHandler extends AbstractHandler{
	protected IProject project;
	
	public AddNatureHandler() {}

	public Object execute(ExecutionEvent event) throws ExecutionException{
		findSelectedProject(event);
		doRun();
		return null;
	}
	
	protected void findSelectedProject(ExecutionEvent event){
		//project = null;
		IStructuredSelection structuredSelection = (IStructuredSelection)HandlerUtil.getCurrentSelection(event); 
		if (structuredSelection.size() == 1) {
			Object object = structuredSelection.getFirstElement();
			if(object instanceof IResource) {
				project = ((IResource)object).getProject();
			} else if(object instanceof IJavaElement) {
				project = ((IJavaElement)object).getJavaProject().getProject();
			}
				
			if(project != null){
				try {
					if (!project.isOpen() || project.hasNature(getNatureID()))
						project = null;
				} catch (CoreException ex) {
					project = null;
					ModelUIPlugin.getPluginLog().logError(ex);
				}
			}
		}
	}
	
	protected void doRun() {
		if(project == null) return;
		RefreshAction refreshAction = new RefreshAction(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell());
		refreshAction.selectionChanged(new StructuredSelection(project));
		refreshAction.run();
				
		IWizard wizard = getWizard(project);
		WizardDialog dialog = new WizardDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
		dialog.open();  
	}
	
	protected abstract IWizard getWizard(IProject project);
	protected abstract String getNatureID();

	protected String findWebXML(String root) {
		if(root == null) return ""; //$NON-NLS-1$
		File rf = new File(root);
		if(!rf.isDirectory()) return ""; //$NON-NLS-1$
		List<File> folders = new ArrayList<File>();
		folders.add(rf);
		return findWebXML(folders);
	}

	protected String findWebXML(List<File> folders) {
		if(folders == null || folders.size() == 0) return ""; //$NON-NLS-1$
		for (File f: folders) {
			if(!f.isDirectory()) continue;
			String s = f.getAbsolutePath().replace('\\', '/') + "/WEB-INF/web.xml"; //$NON-NLS-1$
			if(new File(s).isFile()) return s;
		}
		List<File> nextLevelFolders = new ArrayList<File>();
		for (File f: folders) {
			File[] fs = f.listFiles();
			if(fs == null) continue;
			for (int i = 0; i < fs.length; i++) {
				if(fs[i].isDirectory()) nextLevelFolders.add(fs[i]);
			}
		}
		return findWebXML(nextLevelFolders);
	}
	
	public void dispose() {}

}
