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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.wizards.datatransfer.FileSystemStructureProvider;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.osgi.framework.Bundle;

/**
 * @author eskimo
 * 
 */
public class ResourcesUtils {
	
	public static IProject importProject( 
			Bundle bundle, String templLocation, 
			IProgressMonitor monitor) 
		throws 
			IOException, CoreException, 
			InvocationTargetException, InterruptedException  {
		
		String tplPrjLcStr;
			tplPrjLcStr = FileLocator.resolve(bundle.getEntry(templLocation))
				.getFile();
			IProject importedPrj = createEclipseProject(bundle,tplPrjLcStr,monitor);
			ImportOperation op = new ImportOperation(importedPrj.getFullPath(),
					new File(tplPrjLcStr),
					FileSystemStructureProvider.INSTANCE,
					new IOverwriteQuery() {
						public String queryOverwrite(String pathString) {
							return IOverwriteQuery.ALL;
						}},
					Arrays.asList(new File(tplPrjLcStr).listFiles()));

		op.setCreateContainerStructure(false);
		op.setContext(Display.getCurrent().getActiveShell());
		op.run(monitor);
		Job j = new Job("+++++++++++test"){/* (non-Javadoc)
		 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
		 */
		@Override
		protected IStatus run(IProgressMonitor monitor) {
			System.out.println("++++++++++Running a decorator.job");
			return Status.OK_STATUS;
		}};
		j.setUser(true);
		j.setPriority(Job.DECORATE);
		j.schedule();
		return importedPrj;
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

	public static IProject createEclipseProject(Bundle bundle,
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
