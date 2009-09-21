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
package org.jboss.tools.common.model.ui.views.navigator;

import org.jboss.tools.common.model.ui.action.ModelContributionManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
//import org.eclipse.jface.util.IPropertyChangeListener;
//import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.AddBookmarkAction;
import org.eclipse.ui.actions.AddTaskAction;
import org.eclipse.ui.actions.WorkingSetFilterActionGroup;
import org.eclipse.ui.dialogs.PropertyDialogAction;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.views.navigator.*;

public class ModelNavigatorActionGroup extends ResourceNavigatorActionGroup {

	protected AddBookmarkAction addBookmarkAction;
	protected AddTaskAction addTaskAction;	
	protected PropertyDialogAction propertyDialogAction;
	protected CollapseAllAction collapseAllAction;
	protected ToggleLinkingAction toggleLinkingAction;
	
	protected OpenActionGroup openGroup;
	protected NRefactorActionGroup refactorGroup;
	protected WorkingSetFilterActionGroup workingSetGroup;
	protected WorkspaceActionGroup workspaceGroup;
	
	protected ModelContributionManager mcm;
	protected boolean activateModelContribution = false;

	/**
	 * Constructs the main action group.
	 */
	public ModelNavigatorActionGroup(IResourceNavigator navigator) {
		super(navigator);
		makeSubGroups();
	}

	/**
	 * Makes the actions contained directly in this action group.
	 */
	protected void makeActions() {
		Shell shell = navigator.getSite().getShell();
		
		if(activateModelContribution) {
			mcm = new ModelContributionManager(shell);
		}
		
		addBookmarkAction = new AddBookmarkAction(shell);
		addTaskAction = new AddTaskAction(shell);		
		propertyDialogAction =
			new PropertyDialogAction(navigator.getSite(), navigator.getViewer());
		
		collapseAllAction = new CollapseAllAction(navigator, "Collapse All");
		collapseAllAction.setToolTipText("Collapse All");
		collapseAllAction.setImageDescriptor(getImageDescriptor("elcl16/collapseall.gif")); //$NON-NLS-1$

		toggleLinkingAction = new ToggleLinkingAction(
			navigator, "Link with Editor");
		toggleLinkingAction.setToolTipText("Link with Editor");
		toggleLinkingAction.setImageDescriptor(getImageDescriptor("elcl16/synced.gif"));//$NON-NLS-1$
	}
	
	/**
	 * Makes the sub action groups.
	 */
	protected void makeSubGroups() {
		openGroup = new OpenActionGroup(navigator);
		refactorGroup = new NRefactorActionGroup(navigator);
		workspaceGroup = new WorkspaceActionGroup(navigator);
	}
	
	/**
	 * Extends the superclass implementation to set the context in the subgroups.
	 */
	public void setContext(ActionContext context) {
		super.setContext(context);
		openGroup.setContext(context);
		refactorGroup.setContext(context);
		workspaceGroup.setContext(context);
		if(mcm != null) mcm.setContext(context);
	}
	
	/**
	 * Adds the actions in this group and its subgroups to the action bars.
	 */
	public void fillActionBars(IActionBars actionBars) {
		actionBars.setGlobalActionHandler(
				ActionFactory.PROPERTIES.getId(),
			propertyDialogAction);
		actionBars.setGlobalActionHandler(
				IDEActionFactory.BOOKMARK.getId(),
			addBookmarkAction);
		actionBars.setGlobalActionHandler(
				IDEActionFactory.ADD_TASK.getId(),
			addTaskAction);
		openGroup.fillActionBars(actionBars);
		refactorGroup.fillActionBars(actionBars);
		workspaceGroup.fillActionBars(actionBars);

		IToolBarManager toolBar = actionBars.getToolBarManager();
		toolBar.add(new Separator());
		toolBar.add(collapseAllAction);		
		toolBar.add(toggleLinkingAction);
	}
	
	/**
	 * Updates the actions which were added to the action bars,
	 * delegating to the subgroups as necessary.
	 */
	public void updateActionBars() {
		IStructuredSelection selection =
			(IStructuredSelection) getContext().getSelection();
		propertyDialogAction.setEnabled(selection.size() == 1);
		addBookmarkAction.selectionChanged(selection);
		addTaskAction.selectionChanged(selection);
		
		openGroup.updateActionBars();
		refactorGroup.updateActionBars();
		workspaceGroup.updateActionBars();
		if(mcm != null) mcm.markDirty();
		if(mcm != null) mcm.update(true);
	} 
	
	/**
	 * Runs the default action (open file) by delegating the open group.
	 */
	public void runDefaultAction(IStructuredSelection selection) {
		openGroup.runDefaultAction(selection);
	}
	
	/**
	 * Handles a key pressed event by invoking the appropriate action,
	 * delegating to the subgroups as necessary.
	 */
	public void handleKeyPressed(KeyEvent event) {
		refactorGroup.handleKeyPressed(event);
		workspaceGroup.handleKeyPressed(event);
	}
	
	/**
	 * Extends the superclass implementation to dispose the subgroups.
	 */
	public void dispose() {
		openGroup.dispose();
		refactorGroup.dispose();
		workspaceGroup.dispose();
		if(mcm != null) mcm.dispose();
		super.dispose();
	}
	
	
}
