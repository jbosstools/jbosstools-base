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

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.IDocument;

/**
 * Validation context used in as-you-type validation.
 * @author Alexey Kazakov
 */
public class EditorValidationContext extends ValidationContext {

	private IDocument document;
	private Set<IStringValidator> stringValidators;
	private Set<IJavaElementValidator> javaElementValidators;

	public EditorValidationContext(IProject project, IDocument document) {
		super(project);
		this.document = document;
		stringValidators = new HashSet<IStringValidator>();
		javaElementValidators = new HashSet<IJavaElementValidator>();
		for (IValidator validator : validators) {
			if(validator instanceof IStringValidator) {
				stringValidators.add((IStringValidator)validator);
			}
			if(validator instanceof IJavaElementValidator) {
				javaElementValidators.add((IJavaElementValidator)validator);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.validation.ValidationContext#shouldValidate(org.jboss.tools.common.validation.IValidator, org.eclipse.core.resources.IProject)
	 */
	@Override
	protected boolean shouldValidate(IValidator validator, IProject project) {
		return validator instanceof IAsYouTypeValidator &&
				((IAsYouTypeValidator)validator).shouldValidateAsYouType(project);
	}

	/**
	 * @return the document
	 */
	public IDocument getDocument() {
		return document;
	}

	/**
	 * @return the stringValidators
	 */
	public Set<IStringValidator> getStringValidators() {
		return stringValidators;
	}

	/**
	 * @return the javaElementValidators
	 */
	public Set<IJavaElementValidator> getJavaElementValidators() {
		return javaElementValidators;
	}
}