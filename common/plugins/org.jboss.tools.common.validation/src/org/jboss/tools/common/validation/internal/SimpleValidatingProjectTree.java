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
package org.jboss.tools.common.validation.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.common.validation.IProjectValidationContext;
import org.jboss.tools.common.validation.IValidatingProjectSet;
import org.jboss.tools.common.validation.IValidatingProjectTree;

/**
 * Represents a validating project tree with the only brunch. 
 * @author Alexey Kazakov
 */
public class SimpleValidatingProjectTree implements IValidatingProjectTree, IValidatingProjectSet {

	private Map<IProject, IValidatingProjectSet> brunches = new HashMap<IProject, IValidatingProjectSet>();
	private IValidatingProjectSet brunch;
	private Set<IProject> validatingProjects = new HashSet<IProject>();

	public SimpleValidatingProjectTree(IProject project) {
		addProject(project);
	}

	public SimpleValidatingProjectTree(IValidatingProjectSet brunch) {
		this.brunch = brunch;
		brunches.put(brunch.getRootProject(), brunch);
		validatingProjects.add(brunch.getRootProject());
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectTree#getBrunches()
	 */
	public Map<IProject, IValidatingProjectSet> getBrunches() {
		return brunches;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectSet#getRootProject()
	 */
	public IProject getRootProject() {
		return brunch.getRootProject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectSet#getAllProjests()
	 */
	public Set<IProject> getAllProjects() {
		return brunch.getAllProjects();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectSet#getRootContext()
	 */
	public IProjectValidationContext getRootContext() {
		return brunch.getRootContext();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidatingProjectTree#addProject(org.eclipse.core.resources.IProject)
	 */
	public void addProject(IProject project) {
		if(project!=null && !validatingProjects.contains(project)) {
			Set<IProject> allProjects = new HashSet<IProject>();
			allProjects.add(project);
			brunch = new ValidatingProjectSet(project, allProjects, new ProjectValidationContext());
			brunches.put(brunch.getRootProject(), brunch);
			validatingProjects.add(project);
		}
	}
}