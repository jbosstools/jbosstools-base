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
package org.jboss.tools.foundation.ui.databinding;

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.osgi.util.NLS;

/**
 * A {@link IValidator} that validates ok if the string it shall validate is not
 * empty.
 */
public class SimpleUrlStringValidator implements IValidator {

	/**
	 * 
	 * validates the given url string. Validation passes only if the given value
	 * is not <tt>null</tt> and is a valid url. The url check is done on a
	 * simplified regex.
	 * 
	 * @see SimpleUrlStringChecker#isValid()
	 * 
	 */
	public IStatus validate(Object value) {
		if (!(value instanceof String
				&& new SimpleUrlStringChecker((String) value).isValid())) {
			return ValidationStatus
					.error(NLS.bind("\"{0}\" is not a valid url.", (String) value));
		}
		return ValidationStatus.ok();
	}
}
