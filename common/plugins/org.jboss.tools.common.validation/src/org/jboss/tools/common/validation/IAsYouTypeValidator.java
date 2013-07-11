/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.IRegion;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;

/**
 * Interface to allow for "partial document" as you type validation.
 * @author Alexey Kazakov
 */
public interface IAsYouTypeValidator extends org.jboss.tools.common.validation.IValidator {

	/**
	 * Validates the region.
	 * @param validatorManager
	 * @param rootProject
	 * @param dirtyRegion
	 * @param helper
	 * @param reporter
	 * @param validationContext
	 * @param projectContext
	 * @param file
	 */
	void validate(IValidator validatorManager, IProject rootProject, Collection<IRegion> dirtyRegions, IValidationContext helper, IReporter reporter, EditorValidationContext validationContext, IProjectValidationContext projectContext, IFile file);

	/**
	 * Returns true if the project should be validated as-you-type.
	 * @param project
	 * @return
	 */
	boolean shouldValidateAsYouType(IProject project);
}