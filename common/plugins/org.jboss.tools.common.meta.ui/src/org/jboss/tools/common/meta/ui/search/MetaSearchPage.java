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
package org.jboss.tools.common.meta.ui.search;

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.viewers.*;
import org.eclipse.search.ui.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class MetaSearchPage extends DialogPage implements ISearchPage {
	ISearchPageContainer container;
	Composite composite;
	XEntityData data;
	XAttributeSupport support;

	public boolean performAction() {
///		if (SearchPlugin.useNewSearch())
			return performNewSearch(false);
///		else
///			return performOldSearch();
	}

	public void setContainer(ISearchPageContainer container) {
		this.container = container;
	}

	private ISearchPageContainer getContainer() {
		return container;
	}
	
	public void createControl(Composite parent) {
		data = getSearchEntityData();
		support = new XAttributeSupport(ModelUtilities.getPreferenceModel().getRoot(), data);
		composite = support.createControl(parent);
		composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		setControl(composite);
	}

    private XEntityData getSearchEntityData() {
        String[][] ds = new String[][]{{"SearchValue", "yes"},
                                       {"text to find", "yes"},
                                       {"property name", "no"},
                                       {"ignore case", "no"}};
        XEntityData d = XEntityDataImpl.create(ds);
        XAttributeData[] as = d.getAttributeData();
        for (int i = 0; i < as.length; i++) {
            String n = as[i].getAttribute().getName();
            String v = as[i].getAttribute().getDefaultValue();
            if(v != null) as[i].setValue(v);
        }
        return d;
    }
	private boolean performNewSearch(boolean forground) {
		List scope = null;
		switch (getContainer().getSelectedScope()) {
			case ISearchPageContainer.WORKSPACE_SCOPE:
				IProject[] ps = ModelPlugin.getWorkspace().getRoot().getProjects();
				scope = new ArrayList();
				for (int i = 0; i < ps.length; i++) {
					if(!ps[i].isOpen()) continue;
					scope.add(ps[i]);
				}
				break;
			case ISearchPageContainer.SELECTION_SCOPE:
				scope= getSelectedResourcesScope(false);
				break;
			case ISearchPageContainer.SELECTED_PROJECTS_SCOPE:
				scope= getSelectedResourcesScope(true);
				break;
			case ISearchPageContainer.WORKING_SET_SCOPE:
				IWorkingSet[] workingSets= getContainer().getSelectedWorkingSets();
				scope = new ArrayList(); ///
		}		
		org.eclipse.search.ui.NewSearchUI.activateSearchResultView();
	
		MetaSearchQuery wsJob = new MetaSearchQuery();
		wsJob.setTextToFind(data.getValue("text to find"));
		wsJob.setIgnoreCase("true".equals(data.getValue("ignore case")));
		wsJob.setAttributeMask(data.getValue("property name"));
		wsJob.setScope(scope);
		if (forground) {
			IStatus status= NewSearchUI.runQueryInForeground(getRunnableContext(), wsJob);
			return status != null && status.isOK();
		} else 
			NewSearchUI.runQuery(wsJob);
	
		return true;
	}

	private ISelection getSelection() {
		return container.getSelection();
	}
	private List getSelectedResourcesScope(boolean isProjectScope) {
		List scope = new ArrayList();
		int elementCount= 0;
		IProject firstProject= null;
		if (getSelection() instanceof IStructuredSelection && !getSelection().isEmpty()) {
			Iterator iter= ((IStructuredSelection)getSelection()).iterator();
			while (iter.hasNext()) {
				Object selection= iter.next();

				//Unpack search result entry
				if (selection instanceof ISearchResultViewEntry)
					selection= ((ISearchResultViewEntry)selection).getGroupByKey();

				IResource resource= null;			
				if (selection instanceof IResource)
					resource= (IResource)selection;
				else if (selection instanceof IAdaptable) {
					if (isProjectScope)
						resource= (IProject)((IAdaptable)selection).getAdapter(IProject.class);
					if (resource == null)
						resource= (IResource)((IAdaptable)selection).getAdapter(IResource.class);
				}
				if (resource != null) {

					if (isProjectScope) {
						resource= resource.getProject();
						if (resource == null/* || isProjectScope && scope.encloses(resource)*/)
							continue;
						if (firstProject == null)
							firstProject= (IProject)resource;
					}
					elementCount++;
					scope.add(resource);
				}
			}
		} else if (isProjectScope) {
			IProject editorProject= getEditorProject();
			if (editorProject != null)scope.add(editorProject);
		}
		return scope;
	}

	private IProject getEditorProject() {
		IWorkbenchPart activePart= ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
		if (activePart instanceof IEditorPart) {
			IEditorPart editor= (IEditorPart) activePart;
			IEditorInput input= editor.getEditorInput();
			if (input instanceof IFileEditorInput) {
				return ((IFileEditorInput)input).getFile().getProject();
			}
		}
		return null;
	}

	private IRunnableContext getRunnableContext() {
		IRunnableContext context=  null;
		context= getContainer().getRunnableContext();
		Shell shell= composite.getShell();
		if (context == null)
			context = new ProgressMonitorDialog(shell);
		return context;
	}
	
	private void validate() {
		boolean isValid = getContainer().hasValidScope();
		getContainer().setPerformActionEnabled(true);
	}

	boolean firstTime = false; 
	public void setVisible(boolean visible) {
		if (visible && composite != null) {
			if (firstTime) {
				firstTime = false;
			}
			validate();
		}
		super.setVisible(visible);
	}

}
