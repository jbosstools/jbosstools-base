/*******************************************************************************
 * Copyright (c) 2016 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.ui.internal;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.internal.navigator.resources.nested.HideFolderWhenProjectIsShownAsNested;
import org.eclipse.ui.internal.navigator.resources.nested.HideTopLevelProjectIfNested;
import org.eclipse.ui.internal.navigator.resources.nested.NestedProjectsContentProvider;
import org.eclipse.ui.internal.navigator.resources.nested.ProjectPresentationHandler;
import org.eclipse.ui.navigator.ICommonFilterDescriptor;
import org.eclipse.ui.navigator.INavigatorFilterService;
import org.eclipse.ui.navigator.resources.ProjectExplorer;

public class ProjectExplorerHierarchyInit implements IPartListener {
	private static final QualifiedName applied = new QualifiedName("", "org.jboss.tools.foundation.ui.project.explorer.hierarchy.applied");
	private static final String appliedValue = "true";

	private IWorkbenchPage page;

	public boolean isAlreadyApplied() {
		try {
			String value = ResourcesPlugin.getWorkspace().getRoot().getPersistentProperty(applied);
			return value != null && appliedValue.equals(value);
		} catch (CoreException e) {
			FoundationUIPlugin.pluginLog().logError(e);
		}
		return false;
	}

	void setApplied() {
		try {
			ResourcesPlugin.getWorkspace().getRoot().setPersistentProperty(applied, appliedValue);
		} catch (CoreException e) {
			FoundationUIPlugin.pluginLog().logError(e);
		}
	}

	public void bind(IWorkbenchPage page) {
		page.addPartListener(this);
		this.page = page;
	}

	@Override
	public void partActivated(IWorkbenchPart part) {
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
		if(part instanceof ProjectExplorer) {
			if(isAlreadyApplied()) {
				return;
			}
			setApplied();
			if(page != null) {
				page.removePartListener(this);
			}
			ProjectExplorer explorer = (ProjectExplorer)part;
			boolean previousNest = explorer.getNavigatorContentService().getActivationService().isNavigatorExtensionActive(NestedProjectsContentProvider.EXTENSION_ID);
			boolean newNest = true;
			if (newNest != previousNest) {
				ISelection initialSelection = explorer.getCommonViewer().getSelection();
				INavigatorFilterService filterService = explorer.getNavigatorContentService().getFilterService();
				Set<String> filters = new HashSet<String>();
				for (ICommonFilterDescriptor desc : filterService.getVisibleFilterDescriptors()) {
					if (filterService.isActive(desc.getId())) {
						filters.add(desc.getId());
					}
				}
				if (newNest) {
					explorer.getNavigatorContentService().getActivationService().activateExtensions(new String[] { NestedProjectsContentProvider.EXTENSION_ID }, false);
					filters.add(HideTopLevelProjectIfNested.EXTENSION_ID);
					filters.add(HideFolderWhenProjectIsShownAsNested.EXTENTSION_ID);
				} else {
					explorer.getNavigatorContentService().getActivationService().deactivateExtensions(new String[] { NestedProjectsContentProvider.EXTENSION_ID }, false);
					filters.remove(HideTopLevelProjectIfNested.EXTENSION_ID);
					filters.remove(HideFolderWhenProjectIsShownAsNested.EXTENTSION_ID);
				}
				filterService.activateFilterIdsAndUpdateViewer(filters.toArray(new String[filters.size()]));
				explorer.getNavigatorContentService().getActivationService().persistExtensionActivations();
				explorer.getCommonViewer().refresh();
				explorer.getCommonViewer().setSelection(initialSelection);
			}

			ICommandService s = (ICommandService)explorer.getSite().getService(ICommandService.class);
			Command command = s.getCommand(ProjectPresentationHandler.COMMAND_ID);
			try {
				if(command != null) {
					HandlerUtil.updateRadioState(command, Boolean.toString(newNest));
				} else {
					FoundationUIPlugin.pluginLog().logError("Command " + ProjectPresentationHandler.COMMAND_ID + " is not found.", new Exception());
				}
			} catch (ExecutionException e) {
				FoundationUIPlugin.pluginLog().logError(e);
			}
		}
	}

}
