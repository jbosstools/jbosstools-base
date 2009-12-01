/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

/**
 * @author Alexey Kazakov
 */
public class EclipseUtil {

	public static IJavaProject getJavaProject(IProject project) {
		try {
			if(project == null || !project.isOpen()) return null;
			if(!project.hasNature(JavaCore.NATURE_ID)) return null;
			return JavaCore.create(project);
		} catch (CoreException e) {
			CommonPlugin.getPluginLog().logError(e);
			return null;		
		}
	}

	public static String getJavaProjectOutputLocation(IProject project) {
		IJavaProject javaProject = getJavaProject(project);
		if(javaProject == null) return null;
		try {
			IPath p = javaProject.getOutputLocation();
			IResource r = project.getWorkspace().getRoot().findMember(p);
			return (r == null || r.getLocation() == null) ? null : r.getLocation().toString();
		} catch (CoreException e) {
			CommonPlugin.getPluginLog().logError(e);
			return null;
		}
	}

	public static void addNatureToProject(IProject project, String natureId) throws CoreException {
		IProject proj = project.getProject();
		IProjectDescription description = proj.getDescription();
		String[] prevNatures = description.getNatureIds();
		if (findIndex(prevNatures, natureId) != -1) return; 		
		description.setNatureIds(append(prevNatures, natureId));
		proj.setDescription(description, null);
	}

	public static void removeNatureFromProject(IProject project, String natureId) throws CoreException {
		IProject proj = project.getProject();
		IProjectDescription description = proj.getDescription();
		String[] prevNatures = description.getNatureIds();
		int natureIndex = findIndex(prevNatures, natureId);
		if(natureIndex == -1) return; 				
		description.setNatureIds(remove(prevNatures, natureIndex));
		proj.setDescription(description, null);
	}

	protected static String[] remove(String[] os, int index) {
		String[] ns = new String[os.length - 1];
		System.arraycopy(os, 0, ns, 0, index);
		System.arraycopy(os, index + 1, ns, index, os.length - (index + 1));
		return ns;
	}

	protected static int findIndex(String[] os, String s) {
		for (int i = 0; i < os.length; i++) 
			if(os[i].equals(s)) return i;
		return -1;
	}

	protected static String[] append(String[] os, String s) {
		String[] ns = new String[os.length + 1];
		System.arraycopy(os, 0, ns, 0, os.length);
		ns[os.length] = s;
		return ns;
	}
}