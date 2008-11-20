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
package org.jboss.tools.common.test.util;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.ResourcesUtils;

/**
 * Test plugins may define test projects to be added 
 * to the test workspace to run tests on them.
 * This utility class manages adding/removing test projects.
 * 
 * @author V.Kabanovich
 */

public class TestProjectProvider {
	IProject project;
	boolean makeCopy = false;
	
	/**
	 *  
	 * @param bundleName - name of plugin that contains code of test project.
	 * @param projectPath - path of test project relative to the plugin that
	 * contains it. If null, then by default is computed as '/projects/%name%'
	 * @param name - name of test project. If null, then by default is computed 
	 * as last part of projectPath.
	 * @param makeCopy - boolean flag, if true, then test project will be copied 
	 * to the test workspace; on test completing, the copy is destroyed.
	 * @throws Exception
	 */
	public TestProjectProvider(String bundleName, String projectPath, String name, boolean makeCopy) throws CoreException {
		try {
			if( null == projectPath ) {
				project = ResourcesUtils.importProject(bundleName, "projects" + Path.SEPARATOR + name, null);
			} else {
				project = ResourcesUtils.importProject(bundleName, projectPath, null);
			}
		} catch (IOException e) {
			throw new CoreException(new Status(Status.ERROR,bundleName,e.getMessage(),e));
		} catch (InvocationTargetException e) {
			throw new CoreException(new Status(Status.ERROR,bundleName,e.getMessage(),e));
		} catch (InterruptedException e) {
			throw new CoreException(new Status(Status.ERROR,bundleName,e.getMessage(),e));
		}
	}
	
	public IProject getProject() {
		return project;
	}
	
	public void dispose() {
		if (project == null || !project.exists()) {
			return;
		}
		try {
			boolean oldAutoBuilding = true;
			try {
				oldAutoBuilding = ResourcesUtils.setBuildAutomatically(false);
				JobUtils.waitForIdle(10);
				project.close(null);
				JobUtils.waitForIdle(10);
				project.delete(true, null);
				JobUtils.waitForIdle(10);
			} finally {
				ResourcesUtils.setBuildAutomatically(oldAutoBuilding);
			}
		} catch (CoreException ex) {
			ILog log = Platform.getLog(Platform.getBundle("org.jboss.tools.common.test"));
			IStatus error = new Status(
					IStatus.ERROR,
					"org.jboss.tools.common.test",
					"Exception occurs during project deletion",ex);
			log.log(error);
		}
	}
	
	TestDescriptionFactory tests = null;
	
	public Set<TestDescription> getTestDescriptions() {
		if(tests == null && project != null && project.isOpen()) {
			IFile f = project.getFile(new Path("/testCases.xml"));
			tests = new TestDescriptionFactory(f); 
		}
		return (tests != null) ? tests.getTestDescriptions() : null;
	}
	
	public ArrayList<TestDescription> getTestDescriptions(String name) {
		getTestDescriptions();
		return (tests != null) ? tests.getTestDescriptions(name) : null;
	}

}
