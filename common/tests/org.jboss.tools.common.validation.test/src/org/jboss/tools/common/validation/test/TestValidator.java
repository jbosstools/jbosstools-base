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

import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.jboss.tools.common.preferences.SeverityPreferences;
import org.jboss.tools.common.validation.ContextValidationHelper;
import org.jboss.tools.common.validation.IProjectValidationContext;
import org.jboss.tools.common.validation.IValidatingProjectTree;
import org.jboss.tools.common.validation.IValidator;
import org.jboss.tools.common.validation.ValidationErrorManager;
import org.jboss.tools.common.validation.ValidatorManager;
import org.jboss.tools.common.validation.internal.SimpleValidatingProjectTree;

/**
 * @author Alexey Kazakov
 */
public class TestValidator extends ValidationErrorManager implements IValidator {

	@Override
	public IStatus validate(Set<IFile> changedFiles, IProject project,
			ContextValidationHelper validationHelper,
			IProjectValidationContext validationContext,
			ValidatorManager manager, IReporter reporter)
			throws ValidationException {
		init(project, validationHelper, validationContext, manager, reporter);

		validate();

		return OK_STATUS;
	}

	@Override
	public IStatus validateAll(IProject project,
			ContextValidationHelper validationHelper,
			IProjectValidationContext validationContext,
			ValidatorManager manager, IReporter reporter)
			throws ValidationException {
		init(project, validationHelper, validationContext, manager, reporter);

		validate();

		return OK_STATUS;
	}

	private void validate() {
		throw new RuntimeException("Test excpetion");
	}

	@Override
	public String getId() {
		return "org.jboss.common.validation.test.TestValidator";
	}

	@Override
	public String getBuilderId() {
		return null;
	}

	@Override
	public IValidatingProjectTree getValidatingProjects(IProject project) {
		return new SimpleValidatingProjectTree(project);
	}

	@Override
	public boolean shouldValidate(IProject project) {
		return true;
	}

	@Override
	public boolean isEnabled(IProject project) {
		return true;
	}

	@Override
	protected String getPreference(IProject project, String preferenceKey) {
		return SeverityPreferences.WARNING;
	}

	@Override
	public int getMaxNumberOfMarkersPerFile(IProject project) {
		return 20;
	}

	@Override
	public String getMarkerType() {
		return ValidationErrorManager.DEFAULT_VALIDATION_MARKER;
	}
}