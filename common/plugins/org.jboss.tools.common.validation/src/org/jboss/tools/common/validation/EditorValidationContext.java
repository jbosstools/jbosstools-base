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

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.IDocument;

/**
 * Validation context used in as-you-type validation.
 * @author Alexey Kazakov
 */
public class EditorValidationContext extends ValidationContext {

	private IDocument document;

	public EditorValidationContext(IProject project, IDocument document) {
		super(project);
		this.document = document;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.validation.ValidationContext#shouldValidate(org.jboss.tools.common.validation.IValidator, org.eclipse.core.resources.IProject)
	 */
	@Override
	protected boolean shouldValidate(IValidator validator, IProject project) {
		return validator instanceof IAsYouTypeValidator && super.shouldValidate(validator, project);
	}

	/**
	 * @return the document
	 */
	public IDocument getDocument() {
		return document;
	}
}