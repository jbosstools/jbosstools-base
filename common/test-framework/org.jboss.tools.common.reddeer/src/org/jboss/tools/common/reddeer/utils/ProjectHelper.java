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
import org.eclipse.reddeer.eclipse.core.resources.Project;
import org.eclipse.reddeer.eclipse.jdt.ui.preferences.BuildPathsPropertyPage;
import org.eclipse.reddeer.eclipse.ui.dialogs.PropertyDialog;
import org.eclipse.reddeer.eclipse.ui.navigator.resources.ProjectExplorer;
import org.eclipse.reddeer.workbench.core.condition.JobIsRunning;
import org.eclipse.reddeer.swt.condition.ShellIsAvailable;
import org.eclipse.reddeer.swt.api.Shell;
import org.eclipse.reddeer.swt.api.TreeItem;
import org.eclipse.reddeer.swt.condition.ControlIsEnabled;
import org.eclipse.reddeer.swt.impl.button.PushButton;
import org.eclipse.reddeer.swt.impl.menu.ContextMenuItem;
import org.eclipse.reddeer.swt.impl.shell.DefaultShell;
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
		Project p = pe.getProject(projectName);
		p.select();
		new ContextMenuItem("Refresh").select();
		new WaitWhile(new JobIsRunning(), TimePeriod.LONG);
		PropertyDialog pd =	p.openProperties();
		BuildPathsPropertyPage pPage = new BuildPathsPropertyPage(pd);
		pPage.activateLibrariesTab();
		
		new PushButton(pd, "Add JARs...").click();
		Shell jarSelectionShell = new DefaultShell("JAR Selection");
		List<TreeItem> librariesToAdd = new ArrayList<TreeItem>();
		for (String library : libraryPathMap.keySet()) {
			librariesToAdd.add(new DefaultTreeItem(new DefaultTree(jarSelectionShell), projectName, library));
		}
		new DefaultTree(jarSelectionShell).selectItems(librariesToAdd.toArray(new TreeItem[librariesToAdd.size()]));
		new WaitUntil(new ControlIsEnabled(new PushButton(jarSelectionShell, "OK")));
		new PushButton(jarSelectionShell,"OK").click();
		new WaitWhile(new ShellIsAvailable(jarSelectionShell));
		pd.ok();
		new WaitWhile(new JobIsRunning(), TimePeriod.LONG);

	}

}
