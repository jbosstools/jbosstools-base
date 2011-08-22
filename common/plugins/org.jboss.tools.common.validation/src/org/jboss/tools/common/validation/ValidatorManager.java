 /*******************************************************************************
  * Copyright (c) 2007 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.eclipse.wst.validation.internal.provisional.core.IValidatorJob;
import org.jboss.tools.common.CommonPlugin;

/**
 * This Manager invokes all dependent validators that should be invoked in one job.
 * We need this one because wst validation framework does not let us invoke
 * dependent validators in the same job.
 * @author Alexey Kazakov
 */
public class ValidatorManager implements IValidatorJob {

	private static Set<IProject> validatingProjects = new HashSet<IProject>();
	public static final String SLEEPING = "Sleeping"; //$NON-NLS-1$
	public static final String RUNNING = "Running"; //$NON-NLS-1$
	private static String STATUS = SLEEPING;

	public ValidatorManager() {
		super();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidatorJob#getSchedulingRule(org.eclipse.wst.validation.internal.provisional.core.IValidationContext)
	 */
	public ISchedulingRule getSchedulingRule(IValidationContext helper) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidatorJob#validateInJob(org.eclipse.wst.validation.internal.provisional.core.IValidationContext, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public IStatus validateInJob(IValidationContext helper, IReporter reporter)	throws ValidationException {
		STATUS = RUNNING;
		try {
			ContextValidationHelper validationHelper = (ContextValidationHelper)helper;
			IProject project = validationHelper.getProject();
			if(project==null) {
				return OK_STATUS;
			}
			IValidationContextManager validationContextManager = validationHelper.getValidationContextManager();
			Set<IProject> rootProjects = validationContextManager.getRootProjects();
			IStatus status = OK_STATUS;
			synchronized (validatingProjects) {
				for (IProject rootProject : rootProjects) {
					if(validatingProjects.contains(rootProject)) {
						return OK_STATUS;
					}
					validatingProjects.add(rootProject);
				}
			}
			synchronized (validatingProjects) {
				try {
					validationContextManager.clearValidatedProjectsList();
					Set<IFile> changedFiles = validationHelper.getChangedFiles();
					if(!changedFiles.isEmpty()) {
						status = validate(changedFiles, validationHelper, reporter);
					} else if(!validationContextManager.getRegisteredFiles().isEmpty()) {
						validationContextManager.clearAllResourceLinks();
						status = validateAll(validationHelper, reporter);
					}
				} finally {
					if(validationContextManager!=null) {
						validationContextManager.clearRegisteredFiles();
					}
					validationHelper.cleanup(); // See https://issues.jboss.org/browse/JBIDE-8726
					for (IProject rootProject : rootProjects) {
						validatingProjects.remove(rootProject);
					}
				}
			}
			return status;
		} catch(Exception e) {
			// We need to catch exceptions and wrap them in KBValidationException to let JUnit tests catch validation exceptions reported to eclipse log. 
			CommonPlugin.getDefault().logError(new JBTValidationException(e.getMessage(), e));
			return OK_STATUS;
		} finally {
			STATUS = SLEEPING;
		}
	}

	private IStatus validate(Set<IFile> changedFiles, ContextValidationHelper validationHelper, IReporter reporter) throws ValidationException {
		IValidationContextManager validationContextManager = validationHelper.getValidationContextManager();
		List<IValidator> validators = validationContextManager.getValidators();
		Set<IProject> rootProjects = validationContextManager.getRootProjects();
		removeMarkers(changedFiles);
		for (IValidator validator : validators) {
			for (IProject rootProject : rootProjects) {
				IValidatingProjectSet projectBrunch = validationHelper.getValidationContextManager().getValidatingProjectTree(validator).getBrunches().get(rootProject);
				if(projectBrunch!=null) {
					validator.validate(changedFiles, rootProject, validationHelper, projectBrunch.getRootContext(), this, reporter);
				}
			}
		}
		return OK_STATUS;
	}

	private IStatus validateAll(ContextValidationHelper validationHelper, IReporter reporter) throws ValidationException {
		IValidationContextManager validationContextManager = validationHelper.getValidationContextManager();
		List<IValidator> validators = validationContextManager.getValidators();
		Set<IProject> rootProjects = validationContextManager.getRootProjects();
		removeMarkers(validationHelper.getProjectSetRegisteredFiles());
		for (IValidator validator : validators) {
			for (IProject rootProject : rootProjects) {
				IValidatingProjectSet projectBrunch = validationHelper.getValidationContextManager().getValidatingProjectTree(validator).getBrunches().get(rootProject);
				if(projectBrunch!=null) {
					validator.validateAll(rootProject, validationHelper, projectBrunch.getRootContext(), this, reporter);
				}
			}
		}
		return OK_STATUS;
	}

	private void removeMarkers(Set<IFile> files) {
		try {
			for (IFile file : files) {
				if(file.isAccessible()) {
					file.deleteMarkers(IValidator.KB_PROBLEM_MARKER_TYPE, true, IResource.DEPTH_ZERO);
				}
			}
		} catch (CoreException e) {
			CommonPlugin.getDefault().logError(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidator#cleanup(org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void cleanup(IReporter reporter) {
		reporter = null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidator#validate(org.eclipse.wst.validation.internal.provisional.core.IValidationContext, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void validate(IValidationContext helper, IReporter reporter)	throws ValidationException {
		validateInJob(helper, reporter);
	}

	/**
	 * This method returns a string with status message of the validator. This method is supposed to be used in unit tests.
	 * @return
	 */
	public static String getStatus() {
		return STATUS;
	}

	/**
	 * This method is supposed to be used in unit tests.
	 * @param status
	 */
	public static void setStatus(String status) {
		STATUS = status;
	}
}