/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.test.util;

import junit.extensions.TestSetup;
import junit.framework.Test;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.test.util.xpl.EditorTestHelper;

/**
 * @author eskimo
 *
 */
public class ProjectImportTestSetup extends TestSetup {

	private String bundleName;
	private String[] projectPaths;
	private String[] projectNames;

	/**
	 * @param test
	 */
	public ProjectImportTestSetup(Test test, String bundleName, String projectPath, String projectName) {
		super(test);
		this.bundleName = bundleName;
		this.projectPaths = new String[]{projectPath};
		this.projectNames = new String[]{projectName};
	}

	public ProjectImportTestSetup(Test test, String bundleName, String[] projectPaths, String[] projectNames) {
		super(test);
		this.bundleName = bundleName;
		this.projectPaths = projectPaths;
		this.projectNames = projectNames;
	}

	public IProject importProject() throws Exception {
		return importProjects()[0];
	}

	public IProject[] importProjects() throws Exception {
		IProject[] projects = new IProject[projectPaths.length]; 
		for (int i = 0; i < projectPaths.length; i++) {
			EditorTestHelper.joinBackgroundActivities();
			projects[i] = (IProject)ResourcesUtils.importProject(bundleName, projectPaths[i]);
			EditorTestHelper.joinBackgroundActivities();
		}
		return projects;
	}	

	@Override
	protected void setUp() throws Exception {
		importProjects();
	}

	@Override
	protected void tearDown() throws Exception {
		for (int i = 0; i < projectNames.length; i++) {
			ResourcesUtils.deleteProject(projectNames[i]);
			EditorTestHelper.joinBackgroundActivities();
		}
	}
}