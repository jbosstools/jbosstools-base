/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.sourcelookup;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteDebugSourcePathComputer implements ISourcePathComputerDelegate {

	public static final String ID = "org.jboss.tools.common.jdt.debug.sourceLookup.remoteDebugSourcePathComputer"; //$NON-NLS-1$
	
	public String getId() {
		return ID;
	}
    
	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate#computeSourceContainers(org.eclipse.debug.core.ILaunchConfiguration, org.eclipse.core.runtime.IProgressMonitor)
	 */
	public ISourceContainer[] computeSourceContainers(ILaunchConfiguration configuration, IProgressMonitor monitor) throws CoreException {
		IRuntimeClasspathEntry[] entries = JavaRuntime.computeUnresolvedSourceLookupPath(configuration);
		List<String> projectNames = configuration.getAttribute(RemoteDebugActivator.ATTR_SELECTED_PROJECTS, new ArrayList<String>());
		for (String projectName:projectNames) {
			IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
			if (project != null) {
				IJavaProject javaProject = JavaCore.create(project);
				entries = addProjectEntry(entries, javaProject);
			}
			
		}
		IRuntimeClasspathEntry[] resolved;
		if (RemoteDebugActivator.m2eExists()) {
			resolved = resolveM2eSourceLookupPath(configuration, entries);
		} else {
			resolved = JavaRuntime.resolveSourceLookupPath(entries, configuration);
		}
		
		return JavaRuntime.getSourceContainers(resolved);
	}

	private IRuntimeClasspathEntry[] resolveM2eSourceLookupPath(
			ILaunchConfiguration configuration, IRuntimeClasspathEntry[] entries)
			throws CoreException {
		String projectName = configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, (String) null);
		IRuntimeClasspathEntry[] runtimeResolved = getResolvedEntries(entries, projectName, RemoteDebugActivator.JDT_JAVA_APPLICATION);
		IRuntimeClasspathEntry[] testResolved = getResolvedEntries(entries, projectName, RemoteDebugActivator.JDT_JUNIT_TEST);
		return combineEntries(runtimeResolved, testResolved);
	}

	private IRuntimeClasspathEntry[] combineEntries(
			IRuntimeClasspathEntry[] runtimeResolved,
			IRuntimeClasspathEntry[] testResolved) {
		Set<IRuntimeClasspathEntry> resolved = new LinkedHashSet<IRuntimeClasspathEntry>();
		for (IRuntimeClasspathEntry entry:runtimeResolved) {
			resolved.add(entry);
		}
		for (IRuntimeClasspathEntry entry:testResolved) {
			resolved.add(entry);
		}
		return resolved.toArray(new IRuntimeClasspathEntry[0]);
	}

	private IRuntimeClasspathEntry[] getResolvedEntries(
			IRuntimeClasspathEntry[] entries, String projectName, String typeId)
			throws CoreException {
		ILaunchConfiguration tempConfiguration = RemoteDebugActivator.createTemporaryLaunchConfiguration(projectName, typeId);
		IRuntimeClasspathEntry[] resolved = JavaRuntime.resolveSourceLookupPath(entries, tempConfiguration);
		tempConfiguration.delete();
		return resolved;
	}

	private IRuntimeClasspathEntry[] addProjectEntry(
			IRuntimeClasspathEntry[] entries, IJavaProject javaProject) {
		if (javaProject != null) {
			IRuntimeClasspathEntry runtime = JavaRuntime.newDefaultProjectClasspathEntry(javaProject);
			IRuntimeClasspathEntry[] copy = new IRuntimeClasspathEntry[entries.length + 1];
			System.arraycopy(entries, 0, copy, 0, entries.length);
			copy[entries.length] = runtime;
			entries = copy;
		}
		return entries;
	}

}
