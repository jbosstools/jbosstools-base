/*******************************************************************************
 * Copyright (c) 2016 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.databinding;

import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.IStatus;

/**
 * @author Andre Dietisheim
 */
public class HostValidator implements IValidator {

	private static final Pattern HOST_PATTERN =
			Pattern.compile("(https?://){0,1}[^\\.:0-9]+(\\.[^\\.:0-9]+)*(:[0-9]+){0,1}");

	@Override
	public IStatus validate(Object value) {
		String server = (String) value;
		if (StringUtils.isEmpty(server)) {
			return ValidationStatus.cancel("You have to provide a server to connect to.");
		}
		if (!HOST_PATTERN.matcher(server).matches()) {
			return ValidationStatus.error("You have to provide a valid server to connect to.");
		}
		return ValidationStatus.ok();

	}
}