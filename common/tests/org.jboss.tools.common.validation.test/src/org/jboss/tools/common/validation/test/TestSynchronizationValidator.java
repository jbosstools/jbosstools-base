/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation.test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.jboss.tools.common.validation.ContextValidationHelper;
import org.jboss.tools.common.validation.IProjectValidationContext;
import org.jboss.tools.common.validation.IValidatingProjectSet;
import org.jboss.tools.common.validation.IValidatingProjectTree;
import org.jboss.tools.common.validation.IValidator;
import org.jboss.tools.common.validation.ValidatorManager;
import org.jboss.tools.common.validation.internal.ProjectValidationContext;

/**
 * @author Alexey Kazakov
 */
public class TestSynchronizationValidator implements IValidator {

	private final static TestValidatingProjectTree PROJECT_BRANCH_FOR_CHANGED_PROJECT_1 = new TestValidatingProjectTree(
			new TestValidatingProjectSet(SynchronizationTest.CHANGED_PROJECT_1, SynchronizationTest.PROJECT_A_1),
			new TestValidatingProjectSet(SynchronizationTest.CHANGED_PROJECT_1, SynchronizationTest.CHANGED_PROJECT_B_1, SynchronizationTest.PROJECT_B_2),
			new TestValidatingProjectSet(SynchronizationTest.CHANGED_PROJECT_1, SynchronizationTest.CHANGED_PROJECT_B_1, SynchronizationTest.PROJECT_B_A_1));

	private final static TestValidatingProjectTree PROJECT_BRANCH_FOR_CHANGED_PROJECT_B_1 = new TestValidatingProjectTree(
			new TestValidatingProjectSet(SynchronizationTest.CHANGED_PROJECT_1, SynchronizationTest.CHANGED_PROJECT_B_1, SynchronizationTest.PROJECT_B_A_1),
			new TestValidatingProjectSet(SynchronizationTest.CHANGED_PROJECT_1, SynchronizationTest.CHANGED_PROJECT_B_1, SynchronizationTest.PROJECT_B_2));

	private final static Map<IProject, TestValidatingProjectTree> PROJECT_TREE;
	static {
		PROJECT_TREE = new HashMap<IProject, TestSynchronizationValidator.TestValidatingProjectTree>();
		PROJECT_TREE.put(SynchronizationTest.CHANGED_PROJECT_1, PROJECT_BRANCH_FOR_CHANGED_PROJECT_1);
		PROJECT_TREE.put(SynchronizationTest.CHANGED_PROJECT_B_1, PROJECT_BRANCH_FOR_CHANGED_PROJECT_B_1);
	}

	private final static Set<IProject> VALIDATED_ROOT_PROJECTS = new HashSet<IProject>();

	private static class TestValidatingProjectSet implements IValidatingProjectSet {

		private IProject[] projects;

		public TestValidatingProjectSet(IProject... projects) {
			this.projects = projects;
		}

		public IProject getRootProject() {
			return projects[projects.length-1];
		}

		public Set<IProject> getAllProjects() {
			Set<IProject> result = new HashSet<IProject>();
			for (IProject project : projects) {
				result.add(project);
			}
			return result;
		}

		public IProjectValidationContext getRootContext() {
			return new ProjectValidationContext();
		}
	}

	private static class TestValidatingProjectTree implements IValidatingProjectTree {

		private Map<IProject, IValidatingProjectSet> projects;

		public TestValidatingProjectTree(IValidatingProjectSet... projectBrunches) {
			projects = new HashMap<IProject, IValidatingProjectSet>();
			for (IValidatingProjectSet projectSet : projectBrunches) {
				projects.put(projectSet.getRootProject(), projectSet);
			}
		}

		public Map<IProject, IValidatingProjectSet> getBrunches() {
			return projects;
		}

		public Set<IProject> getAllProjects() {
			Set<IProject> result = new HashSet<IProject>();
			for (IValidatingProjectSet projectSet : projects.values()) {
				result.addAll(projectSet.getAllProjects());
			}
			return result;
		}

		public void addProject(IProject project) {
		}
	}

	private static synchronized void projectHasBeenValidated(IProject project) {
		VALIDATED_ROOT_PROJECTS.add(project);
	}

	public static synchronized void clear() {
		VALIDATED_ROOT_PROJECTS.clear();
	}

	public static synchronized boolean isProjectValidated(IProject project) {
		return VALIDATED_ROOT_PROJECTS.contains(project);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#validate(java.util.Set, org.eclipse.core.resources.IProject, org.jboss.tools.common.validation.ContextValidationHelper, org.jboss.tools.common.validation.IProjectValidationContext, org.jboss.tools.common.validation.ValidatorManager, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	@Override
	public IStatus validate(Set<IFile> changedFiles, IProject project,
			ContextValidationHelper validationHelper,
			IProjectValidationContext validationContext,
			ValidatorManager manager, IReporter reporter)
			throws ValidationException {
		for (IFile file : changedFiles) {
			IProject changedProject = file.getProject();
			validateAll(changedProject, validationHelper, validationContext, manager, reporter);
		}
		return ValidatorManager.OK_STATUS;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#validateAll(org.eclipse.core.resources.IProject, org.jboss.tools.common.validation.ContextValidationHelper, org.jboss.tools.common.validation.IProjectValidationContext, org.jboss.tools.common.validation.ValidatorManager, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	@Override
	public IStatus validateAll(IProject project,
			ContextValidationHelper validationHelper,
			IProjectValidationContext validationContext,
			ValidatorManager manager, IReporter reporter)
			throws ValidationException {
		try {
			Thread.sleep(100);
		} catch (InterruptedException e) {
			// Ignore
		}
		projectHasBeenValidated(project);
		return ValidatorManager.OK_STATUS;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#getId()
	 */
	@Override
	public String getId() {
		return "org.jboss.common.validation.test.TestSynchronizationValidator";
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#getBuilderId()
	 */
	@Override
	public String getBuilderId() {
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#getValidatingProjects(org.eclipse.core.resources.IProject)
	 */
	@Override
	public IValidatingProjectTree getValidatingProjects(IProject project) {
		return PROJECT_TREE.get(project);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#shouldValidate(org.eclipse.core.resources.IProject)
	 */
	@Override
	public boolean shouldValidate(IProject project) {
		return project.getName().startsWith("ChangedProject");
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidator#isEnabled(org.eclipse.core.resources.IProject)
	 */
	@Override
	public boolean isEnabled(IProject project) {
		return shouldValidate(project);
	}
}