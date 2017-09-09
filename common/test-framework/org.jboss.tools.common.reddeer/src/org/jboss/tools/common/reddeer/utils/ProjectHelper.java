/*******************************************************************************
 * Copyright (c) 2016-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.reddeer.common.wait.TimePeriod;
import org.eclipse.reddeer.common.wait.WaitUntil;
import org.eclipse.reddeer.common.wait.WaitWhile;
import org.eclipse.reddeer.core.condition.JobIsRunning;
import org.eclipse.reddeer.core.condition.ShellWithTextIsAvailable;
import org.eclipse.reddeer.eclipse.jdt.ui.ProjectExplorer;
import org.eclipse.reddeer.eclipse.ui.dialogs.ExplorerItemPropertyDialog;
import org.eclipse.reddeer.swt.api.TreeItem;
import org.eclipse.reddeer.swt.condition.WidgetIsEnabled;
import org.eclipse.reddeer.swt.impl.button.PushButton;
import org.eclipse.reddeer.swt.impl.menu.ContextMenu;
import org.eclipse.reddeer.swt.impl.shell.DefaultShell;
import org.eclipse.reddeer.swt.impl.tab.DefaultTabItem;
import org.eclipse.reddeer.swt.impl.tree.DefaultTree;
import org.eclipse.reddeer.swt.impl.tree.DefaultTreeItem;

public class ProjectHelper {
	
	/**
	 * Add specified libraries to project build path
	 * @param projectName name of project to which libraries are added
	 * @param libraryPathMap Map of library path and library name which are to be added to build path.
	 * Library path should end with File.separator 
	 */
	public static void addLibrariesIntoProject(String projectName, Map<String, String> libraryPathMap) {
		
		for(String lib: libraryPathMap.keySet()){
			FileUtils.copyFileIntoProjectFolder(projectName, new File(libraryPathMap.get(lib)+lib));
		}
		
		ProjectExplorer pe = new ProjectExplorer();
		pe.open();
		pe.getProject(projectName).select();
		new ContextMenu("Refresh").select();
		new WaitWhile(new JobIsRunning(), TimePeriod.LONG);

		ExplorerItemPropertyDialog pd = new ExplorerItemPropertyDialog(pe.getProject(projectName));
		pd.open();
		new DefaultShell("Properties for "+projectName);
		new DefaultTreeItem("Java Build Path").select();
		new DefaultTabItem("Libraries").activate();
		
		new PushButton("Add JARs...").click();
		new DefaultShell("JAR Selection");
		List<TreeItem> librariesToAdd = new ArrayList<TreeItem>();
		for (String library : libraryPathMap.keySet()) {
			librariesToAdd.add(new DefaultTreeItem(projectName, library));
		}
		new DefaultTree().selectItems(librariesToAdd.toArray(new TreeItem[librariesToAdd.size()]));
		new WaitUntil(new WidgetIsEnabled(new PushButton("OK")));
		new PushButton("OK").click();
		new WaitWhile(new ShellWithTextIsAvailable("JAR Selection"));
		pd.ok();
		new WaitWhile(new JobIsRunning(), TimePeriod.LONG);

	}

}
