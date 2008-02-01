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

import org.eclipse.core.resources.IProject;
import org.jboss.tools.test.util.xpl.EditorTestHelper;

import junit.extensions.TestSetup;
import junit.framework.Test;

/**
 * @author eskimo
 *
 */
public class ProjectImportTestSetup extends TestSetup {
	
	private String bundleName = "";
	private String projectPath = "";
	private String projectName = "";
	
	/**
	 * @param test
	 */
	public ProjectImportTestSetup(Test test,
			String bundleName, String projectPath, String projectName) {
		super(test);
		this.bundleName = bundleName;
		this.projectPath = projectPath;
		this.projectName = projectName;
	}

	public IProject importProject() throws Exception {
		IProject importedPrj = null;
		EditorTestHelper.joinBackgroundActivities();
		importedPrj = (IProject)ResourcesUtils.importProject(bundleName, projectPath);
		EditorTestHelper.joinBackgroundActivities();
		return importedPrj;
	}	

	@Override
	protected void setUp() throws Exception {
		importProject();
	}

	@Override
	protected void tearDown() throws Exception {
		ResourcesUtils.deleteProject(projectName);
		EditorTestHelper.joinBackgroundActivities();
	}
	
	

}
