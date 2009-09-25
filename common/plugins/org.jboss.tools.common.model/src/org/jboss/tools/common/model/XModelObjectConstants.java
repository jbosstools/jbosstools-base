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
	public static final String ATTR_NAME_BODY                   = "body"; //$NON-NLS-1$
	public static final String ATTR_NAME__BODY_                 = "_body_";	//$NON-NLS-1$
	public static final String ATTR_NAME_HAS_ERRORS             = "_hasErrors_"; //$NON-NLS-1$
	public static final String ATTR_NAME_IS_INCORRECT           = "isIncorrect"; //$NON-NLS-1$
	public static final String ATTR_NAME_ACTUAL_BODY_TIME_STAMP = "actualBodyTimeStamp"; //$NON-NLS-1$
	public static final String ATTR_NAME_INCORRECT_BODY         = "incorrectBody"; //$NON-NLS-1$
	public static final String ATTR_NAME_CORRECT_BODY           = "correctBody"; //$NON-NLS-1$
	public static final String ATTR_NAME_ENCODING               = "encoding"; //$NON-NLS-1$
	public static final String ATTR_NAME_SYSTEM_ID              = "systemId"; //$NON-NLS-1$
	public static final String ATTR_NAME_PUBLIC_ID              = "publicId"; //$NON-NLS-1$

	public static final String ATTR_NAME_ERRORS                 = "errors"; //$NON-NLS-1$
	public static final String ATTR_NAME_OVERLAPPED             = "overlapped"; //$NON-NLS-1$
	public static final String ATTR_NAME_OVERLAPPED_SYSTEM      = "overlappedSystem"; //$NON-NLS-1$

	public static final String ATTR_NAME_NAMESPACE              = "namespace"; //$NON-NLS-1$
	public static final String ATTR_NAME_VERSION                = "version"; //$NON-NLS-1$
	public static final String ATTR_NAME_STANDALONE             = "standalone"; //$NON-NLS-1$
	
	public static final String ATTR_NAME_FORCE_LOAD             = "forceLoad"; //$NON-NLS-1$

	public static final String ATTR_NAME__FILE                  = "_file"; //$NON-NLS-1$

	public static final String ATTR_NAME_EXTENSION              = "extension"; //$NON-NLS-1$
	public static final String ATTR_NAME_LOCATION               = "location"; //$NON-NLS-1$
	
	public static final String ATTRIBUTE_VALUE_STR_YES          = "yes"; //$NON-NLS-1$
	
	public static final String ENT_FILE_ANY_LONG                = "FileAnyLong"; //$NON-NLS-1$
	public static final String ENT_FILE_SYSTEM_FOLDER           = "FileSystemFolder"; //$NON-NLS-1$

	public static String SEPARATOR = "/"; //$NON-NLS-1$
	public static String YES = "yes"; //$NON-NLS-1$
	public static String NO = "no"; //$NON-NLS-1$
	public static String TRUE = "true"; //$NON-NLS-1$
	public static String FALSE = "false"; //$NON-NLS-1$
	
	public static String ATTR_NAME = "name"; //$NON-NLS-1$
	public static String XML_ATTR_NAME = "NAME"; //$NON-NLS-1$
	public static String ATTR_ELEMENT_TYPE = "element type"; //$NON-NLS-1$

	public static String PROJECT = "project"; //$NON-NLS-1$

	public static String PROP_ROOT_ENTITY = "rootEntity"; //$NON-NLS-1$
	public static String ROOT_OBJECT = "Root"; //$NON-NLS-1$
	// ""
	
	// "setModified" use ENUM for compare

	//drop & palette related 
    public static final String START_TEXT = "start text"; //$NON-NLS-1$
    public static final String END_TEXT = "end text"; //$NON-NLS-1$
    public static final String REFORMAT = "automatically reformat tag body"; //$NON-NLS-1$
}
