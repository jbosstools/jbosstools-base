/*******************************************************************************
  * Copyright (c) 2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.el.core.refactoring;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;

/**
 * 
 * @author Daniel
 * 
 * This interface is used for transmit information about seam project structure from seam plugins through org.jboss.tools.common.el.core.elSearcher extension point
 */
public interface ProjectsSet {
	/**
	 *  inits seam project structure 
	 * @param project
	 */
	public void init(IProject project);
	
	/**
	 *  returns all linked seam projects
	 * @return
	 */
	public IProject[] getLinkedProjects();
	
	/**
	 *  returns view folder (like WEB_CONTENT, EAR_CONTENT) for each seam project
	 * @param project
	 * @return
	 */
	public IContainer getViewFolder(IProject project);
}
