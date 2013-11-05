/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
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
import org.jboss.tools.common.validation.ContextValidationHelper;
import org.jboss.tools.common.validation.IProjectValidationContext;
import org.jboss.tools.common.validation.IValidatingProjectTree;
import org.jboss.tools.common.validation.ValidatorManager;
import org.jboss.tools.common.validation.internal.SimpleValidatingProjectTree;

/**
 * @author Alexey Kazakov
 */
public class TestContextValidator extends TestValidator {

	private static IValidatingProjectTree tree;
	private static Integer counter = 0;
	private static Boolean enabled = false;

	@Override
	public IStatus validate(Set<IFile> changedFiles, IProject project,
			ContextValidationHelper validationHelper,
			IProjectValidationContext validationContext,
			ValidatorManager manager, IReporter reporter)
			throws ValidationException {
		init(project, validationHelper, validationContext, manager, reporter);
		synchronized (counter) {
			counter++;
		}

		return OK_STATUS;
	}

	public static int getCounter() {
		synchronized(counter) {
			return counter.intValue();
		}
	}

	@Override
	public IStatus validateAll(IProject project,
			ContextValidationHelper validationHelper,
			IProjectValidationContext validationContext,
			ValidatorManager manager, IReporter reporter)
			throws ValidationException {
		return OK_STATUS;
	}

	@Override
	public String getId() {
		return "org.jboss.common.validation.test.TestContextValidator";
	}

	@Override
	public synchronized IValidatingProjectTree getValidatingProjects(IProject project) {
		if(tree == null) {
			tree = new SimpleValidatingProjectTree(project);
		}
		return tree;
	}

	@Override
	public boolean shouldValidate(IProject project) {
		synchronized(enabled) {
			return enabled && "JavaProject".equals(project.getName());			
		}
	}

	public static void enable(boolean enableValidation) {
		enabled = enableValidation;
	}

	@Override
	public boolean isEnabled(IProject project) {
		return shouldValidate(project);
	}
}