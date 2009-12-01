/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/  
package org.jboss.tools.common.ui;

import java.util.Map;

import org.eclipse.core.runtime.IStatus;

/**
 * Simple validator interface
 * @author eskimo
 *
 */
public interface IValidator {
	
	public static final String DEFAULT_ERROR = "default.error"; //$NON-NLS-1$
	
	/**
	 * Method perform validation of given value against a provided context. 
	 * @param value - an object that should be validated against context
	 * @param context - a contextual data that can be used during validation
	 * @return map from editor name to error text
	 */
	Map<String, IStatus> validate(Object value, Object context);
}
