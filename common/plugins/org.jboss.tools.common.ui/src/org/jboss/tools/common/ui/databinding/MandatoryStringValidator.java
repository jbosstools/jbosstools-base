/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.IStatus;

/**
 * A {@link IValidator} that validates ok if the string it shall validate is not empty.
 */
public class MandatoryStringValidator implements IValidator {

	private String errorMessage;

	public MandatoryStringValidator(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 * 
	 * validates the given string. Validation passes only if the given value is
	 * not <tt>null</tt> and it's length's larger than 0
	 * 
	 */
	public IStatus validate(Object value) {
		if (!((value instanceof String) && ((String) value).length() > 0)) {
			return ValidationStatus.error(errorMessage);
		}
		return ValidationStatus.ok();
	}
}
