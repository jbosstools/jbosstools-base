/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model;

/**
 * @author eskimo
 */

public interface XModelObjectConstants {
	public static final String ATTR_NAME_BODY                   = "body";
	public static final String ATTR_NAME__BODY_                 = "_body_";	
	public static final String ATTR_NAME_HAS_ERRORS             = "_hasErrors_";
	public static final String ATTR_NAME_IS_INCORRECT           = "isIncorrect";
	public static final String ATTR_NAME_ACTUAL_BODY_TIME_STAMP = "actualBodyTimeStamp";
	public static final String ATTR_NAME_INCORRECT_BODY         = "incorrectBody";
	public static final String ATTR_NAME_CORRECT_BODY           = "correctBody";	
	public static final String ATTR_NAME_ENCODING               = "encoding";
	public static final String ATTR_NAME_SYSTEM_ID              = "systemId";	
	public static final String ATTR_NAME_PUBLIC_ID              = "publicId";

	public static final String ATTR_NAME_ERRORS                 = "errors";
	public static final String ATTR_NAME_OVERLAPPED             = "overlapped";
	public static final String ATTR_NAME_OVERLAPPED_SYSTEM      = "overlappedSystem";

	public static final String ATTR_NAME_NAMESPACE              = "namespace";
	public static final String ATTR_NAME_VERSION                = "version";
	public static final String ATTR_NAME_STANDALONE             = "standalone";
	
	public static final String ATTR_NAME_FORCE_LOAD             = "forceLoad";	
	
	public static final String ATTRIBUTE_VALUE_STR_YES = "yes";
	// public static final String ATTRIBUTE_VALUE_STR_NO  = "no"
	// "true"
	// "false"
	// ""
	
	// "setModified" use ENUM for compare
}
