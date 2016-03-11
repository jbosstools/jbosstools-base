/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.internal.junit.ui.TestRunnerViewPart;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.internal.cheatsheets.ICheatSheetResource;
import org.eclipse.ui.navigator.resources.ProjectExplorer;
import org.eclipse.ui.progress.IProgressConstants;

/**
 * @author Alexey Kazakov
 */
public class JBossPerspectiveFactory implements IPerspectiveFactory {

	public static final String PERSPECTIVE_ID = "org.jboss.tools.common.ui.JBossPerspective"; //$NON-NLS-1$

	protected static final String ID_SERVERS_VIEW = "org.eclipse.wst.server.ui.ServersView"; //$NON-NLS-1$
	protected static final String ID_SEARCH_VIEW = "org.eclipse.search.ui.views.SearchView"; //$NON-NLS-1$
	protected static final String ID_CONSOLE_VIEW = "org.eclipse.ui.console.ConsoleView"; //$NON-NLS-1$
	protected static final String ID_CHEATSHEET_VIEW = "org.eclipse.ui.cheatsheets.cheatSheetView"; //$NON-NLS-1$

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
	 */
	@Override
	public void createInitialLayout(IPageLayout layout) {
		layout.addActionSet("org.eclipse.jst.j2ee.J2eeMainActionSet"); //$NON-NLS-1$
		layout.addActionSet(JavaUI.ID_ACTION_SET);
		layout.addActionSet(JavaUI.ID_ELEMENT_CREATION_ACTION_SET);

		layout.addActionSet(IDebugUIConstants.LAUNCH_ACTION_SET);
		layout.addActionSet(IDebugUIConstants.DEBUG_ACTION_SET);

		layout.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);

		layout.addShowViewShortcut(ProjectExplorer.VIEW_ID);
		layout.addShowViewShortcut(JavaUI.ID_PACKAGES);
		layout.addShowViewShortcut(JavaUI.ID_TYPE_HIERARCHY);
		layout.addShowViewShortcut(JavaUI.ID_SOURCE_VIEW);
		layout.addShowViewShortcut(ID_SERVERS_VIEW);
		layout.addShowViewShortcut(IPageLayout.ID_BOOKMARKS);
		layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
		layout.addShowViewShortcut(IPageLayout.ID_PROP_SHEET);
		layout.addShowViewShortcut(IPageLayout.ID_RES_NAV);
		layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);


		// views - search
		layout.addShowViewShortcut(ID_SEARCH_VIEW);
		// views - debugging
		layout.addShowViewShortcut(ID_CONSOLE_VIEW);

		layout.addShowInPart(ProjectExplorer.VIEW_ID);

		Set<String> knownViews = new HashSet<String>();

		String editorArea = layout.getEditorArea();

		// Top left.
		IFolderLayout topLeft = layout.createFolder("topLeft", IPageLayout.LEFT, 0.25f, editorArea);//$NON-NLS-1$
		topLeft.addView(add(ProjectExplorer.VIEW_ID, knownViews));
		topLeft.addView(add(JavaUI.ID_PACKAGES, knownViews));
		topLeft.addPlaceholder(add(IPageLayout.ID_RES_NAV, knownViews));
		topLeft.addPlaceholder(add(JavaUI.ID_TYPE_HIERARCHY, knownViews));
		topLeft.addPlaceholder(add(JavaUI.ID_PACKAGES_VIEW, knownViews));
		topLeft.addPlaceholder(add(TestRunnerViewPart.NAME, knownViews));

		// Bottom right.
		IFolderLayout bottomRight = layout.createFolder("bottomRight", IPageLayout.BOTTOM, 0.7f, editorArea);//$NON-NLS-1$
		bottomRight.addView(add(IPageLayout.ID_PROBLEM_VIEW, knownViews));
//		bottomRight.addView(IPageLayout.ID_PROP_SHEET);
		bottomRight.addView(add(ID_SERVERS_VIEW, knownViews));

		IFolderLayout bottomRight2 = layout.createFolder("bottomRight2", IPageLayout.RIGHT , 0.6f, "bottomRight");//$NON-NLS-1$
		bottomRight2.addView(add(IPageLayout.ID_PROP_SHEET, knownViews));

		bottomRight.addPlaceholder(add(IPageLayout.ID_TASK_LIST, knownViews));
		bottomRight.addPlaceholder(add(ID_CONSOLE_VIEW, knownViews));
		bottomRight.addPlaceholder(add(IPageLayout.ID_BOOKMARKS, knownViews));
		bottomRight.addPlaceholder(add(IProgressConstants.PROGRESS_VIEW_ID, knownViews));
		bottomRight.addPlaceholder(add(ID_SEARCH_VIEW, knownViews));

		// Top right.
		IFolderLayout topRight = layout.createFolder("topRight", IPageLayout.RIGHT, 0.7f, editorArea);//$NON-NLS-1$
		topRight.addView(add(IPageLayout.ID_OUTLINE, knownViews));

		assignAllUnknownViewsToDefaultFolder(bottomRight, knownViews);

		// new actions - Java project creation wizard
		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewPackageCreationWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewClassCreationWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewInterfaceCreationWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewSourceFolderCreationWizard");	 //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewSnippetFileCreationWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");//$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");//$NON-NLS-1$
	}

	/**
	 * Adds viewId to the set and returns it for further usage.
	 * @param viewId
	 * @param views
	 * @return passed viewId
	 */
	private String add(String viewId, Set<String> views) {
		views.add(viewId);
		return viewId;
	}

	static final String PERSPECTIVE_EXTENSION = "org.eclipse.ui.perspectiveExtensions";
	static final String VIEW_EXTENSION = "org.eclipse.ui.views";
	static final String ATTR_TARGET_ID = "targetID";
	static final String NODE_VIEW = "view";
	static final String ATTR_ID = "id";

	/**
	 * Assigns all views that were not assigned in explicitely or in perspective extension to the specified folder.
	 * That overrides Eclipse default choice of most right bottom folder.
	 * 
	 * @param folder
	 * @param knownViews
	 */
	private void assignAllUnknownViewsToDefaultFolder(IFolderLayout folder, Set<String> knownViews) {
		// Add to the set id of views that are assigned to folders in perspective extensions for this perspective. 
		for (IExtension e: Platform.getExtensionRegistry().getExtensionPoint(PERSPECTIVE_EXTENSION).getExtensions()) {
			for (IConfigurationElement c1: e.getConfigurationElements()) {
				if(PERSPECTIVE_ID.equals(c1.getAttribute(ATTR_TARGET_ID))) {
					for (IConfigurationElement c2: c1.getChildren(NODE_VIEW)) {
						String id = c2.getAttribute(ATTR_ID);
						if(id != null) {
							knownViews.add(id);
						}
					}
				}
			}
		}
		// Now assign all other declared views to the desired folder.
		for (IExtension e: Platform.getExtensionRegistry().getExtensionPoint(VIEW_EXTENSION).getExtensions()) {
			for (IConfigurationElement c: e.getConfigurationElements()) {
				if(NODE_VIEW.equals(c.getName())) {
					String id = c.getAttribute(ATTR_ID);
					if(id != null && !knownViews.contains(id)) {
						folder.addPlaceholder(id);
					}
				}
			}
		}
	}

}