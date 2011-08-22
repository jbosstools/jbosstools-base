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
package org.jboss.tools.common.validation.internal;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.common.validation.IProjectValidationContext;
import org.jboss.tools.common.validation.IValidatingProjectSet;

/**
 * @author Alexey Kazakov
 */
public class ValidatingProjectSet implements IValidatingProjectSet {

	protected IProject rootProject;
	protected Set<IProject> allProjects;
	protected IProjectValidationContext rootContext;

	protected ValidatingProjectSet() {
	}

	/**
	 * @param rootProject
	 * @param allProjects
	 * @param rootContext
	 */
	public ValidatingProjectSet(IProject rootProject, Set<IProject> allProjects, IProjectValidationContext rootContext) {
		this.rootProject = rootProject;
		this.allProjects = allProjects;
		this.rootContext = rootContext;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectSet#getAllProjests()
	 */
	public Set<IProject> getAllProjects() {
		return allProjects;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectSet#getRootContext()
	 */
	public IProjectValidationContext getRootContext() {
		return rootContext;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectSet#getRootProject()
	 */
	public IProject getRootProject() {
		return rootProject;
	}
}