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

package org.jboss.tools.common.core.classpath;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

/**
 * 
 * @author Rob Stryker <rob.stryker@redhat.com>
 * 
 */
public abstract class AbstractClasspathContainerInitializer extends
		ClasspathContainerInitializer {
	
	protected IJavaProject javaProject;
	public AbstractClasspathContainerInitializer() {
	}

	/**
	 * Description of the Method
	 * 
	 * @param containerPath
	 *            Description of the Parameter
	 * @param project
	 *            Description of the Parameter
	 * @exception CoreException
	 *                Description of the Exception
	 */
	public void initialize(IPath containerPath, IJavaProject project)
			throws CoreException {
		this.javaProject = project;
		int size = containerPath.segmentCount();
		if (size > 0) {
			AbstractClasspathContainer container = createClasspathContainer(containerPath);
			JavaCore.setClasspathContainer(containerPath,
				new IJavaProject[] { project },
				new IClasspathContainer[] { container }, null);
		}
	}

	/**
	 * Description of the Method
	 * 
	 * @param path
	 *            Description of the Parameter
	 * @return Description of the Return Value
	 */
	protected abstract AbstractClasspathContainer createClasspathContainer(
			IPath path);

	/**
	 * Gets the classpathContainerID attribute of the
	 * AbstractClasspathContainerInitializer object
	 * 
	 * @return The classpathContainerID value
	 */
	protected abstract String getClasspathContainerID();

	public boolean canUpdateClasspathContainer(IPath containerPath,
			IJavaProject project) {
		return true;
	}

	public void requestClasspathContainerUpdate(final IPath containerPath,
			final IJavaProject project, final IClasspathContainer sg)

	throws CoreException

	{
		String key = AbstractClasspathContainer
				.getDecorationManagerKey(containerPath.toString());

		IClasspathEntry[] entries = sg.getClasspathEntries();
		ClasspathDecorationsManager decorations = AbstractClasspathContainer
				.getDecorationsManager();
		decorations.clearAllDecorations(key);

		for (int i = 0; i < entries.length; i++) {
			final IClasspathEntry entry = entries[i];

			final IPath srcpath = entry.getSourceAttachmentPath();
			final IPath srcrootpath = entry.getSourceAttachmentRootPath();
			final IClasspathAttribute[] attrs = entry.getExtraAttributes();
			final String eid = entry.getPath().toString();
			final ClasspathDecorations dec = new ClasspathDecorations();

			dec.setSourceAttachmentPath(srcpath);
			dec.setSourceAttachmentRootPath(srcrootpath);
			dec.setExtraAttributes(attrs);

			decorations.setDecorations(key, eid, dec);
		}
		decorations.save();
		final IClasspathContainer container = JavaCore.getClasspathContainer(
				containerPath, project);
		((AbstractClasspathContainer) container).refresh();
	}

}
