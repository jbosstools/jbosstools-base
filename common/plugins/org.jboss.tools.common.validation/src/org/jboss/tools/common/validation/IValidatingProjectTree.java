/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;

/**
 * Represents a tree of related projects which are being validated.
 * 
 * @author Alexey Kazakov
 */
public interface IValidatingProjectTree {

	/**
	 * Returns the the map of chains of dependent projects. We can call treat such chains of projects as brunches of the tree.
	 * Each chain ends with a root project which depends (directly or indirectly) on all the project in the brunch there is not other projects which depends on it.
	 * Map key is a root project, map value is a brunch for the root project.
	 *  
	 * @return
	 */
	Map<IProject, IValidatingProjectSet> getBrunches();

	/**
	 * @return all the projects of the tree.
	 */
	Set<IProject> getAllProjects();

	/**
	 * Add brunches for of the project.
	 * @param project
	 */
	void addProject(IProject project);
}