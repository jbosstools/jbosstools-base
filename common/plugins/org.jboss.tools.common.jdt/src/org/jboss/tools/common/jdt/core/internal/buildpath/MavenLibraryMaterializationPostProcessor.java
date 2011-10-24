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
package org.jboss.tools.common.jdt.core.internal.buildpath;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.jdt.core.buildpath.ILibraryMaterializationPostProcessor;

class MavenLibraryMaterializationPostProcessor implements ILibraryMaterializationPostProcessor {

	private static final String MAVEN_NATURE_ID = "org.eclipse.m2e.core.maven2Nature";

	public boolean applies(IJavaProject javaProject, IPath containerPath) throws CoreException {
		boolean applies = javaProject != null
				&& javaProject.getProject().hasNature(MAVEN_NATURE_ID)
				&& ("org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER"
						.equals(containerPath.toPortableString()));
		return applies;
	}

	public void execute(IJavaProject javaProject, IPath containerPath,
			IProgressMonitor monitor) throws CoreException {
		if (applies(javaProject, containerPath)) {
			removeExclusionPatterns(javaProject, monitor);
			EclipseUtil.removeNatureFromProject(javaProject.getProject(), MAVEN_NATURE_ID);
		}
	}

	private void removeExclusionPatterns(IJavaProject javaProject, IProgressMonitor monitor) throws JavaModelException {
		IClasspathEntry[] entries = javaProject.getRawClasspath();
		ArrayList<IClasspathEntry> newEntriesList = new ArrayList<IClasspathEntry>(entries.length);
		for (IClasspathEntry entry : entries) {
			if (entry.getEntryKind() == IClasspathEntry.CPE_SOURCE) {
				IPath[] newExclusionPatterns = getExclusionPatterns(entry.getExclusionPatterns());
				IClasspathEntry newEntry = JavaCore.newSourceEntry(	entry.getPath(), 
																	entry.getInclusionPatterns(),
																	newExclusionPatterns, 
																	entry.getOutputLocation(),
																	entry.getExtraAttributes());
				newEntriesList.add(newEntry);
			} else {
				newEntriesList.add(entry);
			}
		}

		IClasspathEntry[] newEntries = new IClasspathEntry[newEntriesList.size()];
		newEntriesList.toArray(newEntries);
		javaProject.setRawClasspath(newEntries, monitor);
	}

	private IPath[] getExclusionPatterns(IPath[] existingPatterns ) {
		ArrayList<IPath> exclusionPatterns = new ArrayList<IPath>(existingPatterns.length);
		for (IPath p : existingPatterns) {
			if (!"**".equals(p.toPortableString())) {
				exclusionPatterns.add(p);
			}
		}
		IPath[] newExclusionPatterns = new IPath[exclusionPatterns.size()];
		exclusionPatterns.toArray(newExclusionPatterns);
		return newExclusionPatterns;
	}
}
