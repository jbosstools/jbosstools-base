/*******************************************************************************
 * Copyright (c) 2018 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;

/**
 * A validator that checks that:
 * - name is not empty
 * - Eclipse project does not already exists
 *
 */
public class EclipseProjectValidator extends MandatoryStringValidator {

	private String existingErrorMessage;

	/**
	 * @param emptyErrorMessage
	 */
	public EclipseProjectValidator(String emptyErrorMessage, String existingErrorMessage) {
		super(emptyErrorMessage);
		this.existingErrorMessage = existingErrorMessage;
	}

	@Override
	public IStatus validate(Object value) {
		IStatus status = super.validate(value);
		if (status.isOK()) {
			String projectName = (String) value;
			if (ResourcesPlugin.getWorkspace().getRoot().getProject(projectName).exists()) {
				status = ValidationStatus.error(existingErrorMessage);
				
			}
		}
		return status;
	}

}
