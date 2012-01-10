/*************************************************************************************
 * Copyright (c) 2008-2012 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.core.internal.buildpath;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IJavaProject;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.jdt.core.buildpath.ClasspathContainersHelper;
import org.jboss.tools.common.jdt.core.buildpath.ILibraryMaterializationPostProcessor;

class GradleLibraryMaterializationPostProcessor implements ILibraryMaterializationPostProcessor {

	private static final String GRADLE_NATURE_ID = "com.springsource.sts.gradle.core.nature";

	public boolean applies(IJavaProject javaProject, IPath containerPath) throws CoreException {
		boolean applies = javaProject != null
				&& javaProject.getProject().hasNature(GRADLE_NATURE_ID)
				&& (ClasspathContainersHelper.GRADLE_CONTAINER_ID
						.equals(containerPath.toPortableString()));
		return applies;
	}

	public void execute(IJavaProject javaProject, IPath containerPath,
			IProgressMonitor monitor) throws CoreException {
		if (applies(javaProject, containerPath)) {
			EclipseUtil.removeNatureFromProject(javaProject.getProject(), GRADLE_NATURE_ID);
		}
	}
}
