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
package org.jboss.tools.common.validation;

import org.eclipse.osgi.util.NLS;

/**
 * @author Alexey Kazakov
 */
public class ValidationMessages {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.validation.Validation"; //$NON-NLS-1$

	public static String VALIDATION_CONTEXT_LINKED_RESOURCE_PATH_MUST_NOT_BE_NULL;
	public static String VALIDATION_CONTEXT_VARIABLE_NAME_MUST_NOT_BE_NULL;
	
	public static String ILLEGAL_CONTENTTYPE;
	
	public static String EXCEPTION_DURING_CREATING_MARKER;

	public static String ERR_ILLIGAL_VALIDATION_STATE;

	static {
		NLS.initializeMessages(BUNDLE_NAME, ValidationMessages.class);
	}
}