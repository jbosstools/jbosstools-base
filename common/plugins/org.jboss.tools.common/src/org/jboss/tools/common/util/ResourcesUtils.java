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
package org.jboss.tools.common.util;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.wizards.datatransfer.FileSystemStructureProvider;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.osgi.framework.Bundle;

/**
 * @author eskimo
 * 
 */
public class ResourcesUtils {

	public static IProject importProject(Bundle bundle, String templLocation,
			IProgressMonitor monitor) throws IOException, CoreException,
			InvocationTargetException, InterruptedException {

		String tplPrjLcStr;
		tplPrjLcStr = FileLocator.resolve(bundle.getEntry(templLocation))
				.getFile();

		return importProject(tplPrjLcStr, monitor);
	}

	public static IProject importProject(String location,
			IProgressMonitor monitor) throws CoreException, IOException,
			InvocationTargetException, InterruptedException {
		IProject importedPrj = createEclipseProjectDromDescriptor(location,
				monitor);
		ImportOperation op = new ImportOperation(importedPrj.getFullPath(),
				new File(location), FileSystemStructureProvider.INSTANCE,
				new IOverwriteQuery() {
					public String queryOverwrite(String pathString) {
						return IOverwriteQuery.ALL;
					}
				}, Arrays.asList(new File(location).listFiles()));

		op.setCreateContainerStructure(false);
		op.run(monitor);
		return importedPrj;
	}

	public static IProject importExistingProject(IProject project, String location, String name) throws CoreException {
		return importExistingProject(project, location, name, new NullProgressMonitor(), true);
	}

	public static IProject importExistingProject(IProject project, String location, String name, IProgressMonitor monitor, boolean refreshWorkspace) throws CoreException {
		IPath path = new Path(location).append(".project"); //$NON-NLS-1$
		IProjectDescription description = ResourcesPlugin.getWorkspace().loadProjectDescription(path);
		description.setName(name);
		project.create(description, monitor);
		project.open(IResource.NONE, monitor);
		if(refreshWorkspace) {
			ResourcesPlugin.getWorkspace().getRoot().refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		}
		return project;
	}

	public static IProject createEclipseProject(String projectName,
			IProgressMonitor monitor) throws CoreException {

		IProject newProjectHandle = ResourcesPlugin.getWorkspace().getRoot()
				.getProject(projectName);

		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IProjectDescription description = workspace
				.newProjectDescription(projectName);
		newProjectHandle.create(description, new NullProgressMonitor());
		newProjectHandle.open(monitor);
		return newProjectHandle;
	}

	public static IProject createEclipseProjectDromDescriptor(
			String templateLocation, IProgressMonitor monitor)
			throws CoreException, IOException {

		IPath tplPrjDescr = new Path(templateLocation)
				.append(IProjectDescription.DESCRIPTION_FILE_NAME);
		IProjectDescription descr = ResourcesPlugin.getWorkspace()
				.loadProjectDescription(tplPrjDescr);
		descr.setLocation(null);
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(
				descr.getName());
		project.create(descr, monitor);
		project.open(IResource.BACKGROUND_REFRESH, monitor);

		return project;
	}
}
