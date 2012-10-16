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
package org.jboss.tools.common.ui;

import org.eclipse.osgi.util.NLS;

public final class CommonUIMessages extends NLS {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.ui.messages";//$NON-NLS-1$

	private CommonUIMessages() {
		// Do not instantiate
	}

	public static String SWT_FIELD_EDITOR_FACTORY_ERROR;

	public static String SWT_FIELD_EDITOR_FACTORY_BROWS;

	public static String VALIDATOR_FACTORY_PATH_TO_A_FOLDER_CANNOT_BE_NULL;

	public static String VALIDATOR_FACTORY_FOLDER_DOES_NOT_EXIST;

	public static String VALIDATOR_FACTORY_PATH_POINTS_TO_FILE;

	public static String SELECT_WORKSPACE_FOLDER_DIALOG_TITLE;

	public static String SELECT_WORKSPACE_FOLDER_DIALOG_MESSAGE;

	public static String SWT_FIELD_EDITOR_FACTORY_OK;

	public static String SWT_FIELD_EDITOR_FACTORY_SELECT_SEAM_HOME_FOLDER;

	public static String SWT_FIELD_EDITOR_FACTORY_NOT_IMPLEMENTED_YET;

	public static String BASE_FIELD_EDITOR_EDITOR_SUPPORTS_ONLY_GRID_LAYOUT;

	public static String BASE_FIELD_EDITOR_NO_LABEL;

	public static String BASE_FIELD_EDITOR_PARENT_CONTROL_SHOULD_BE_COMPOSITE;

	public static String BASE_FIELD_EDITOR_PARENT_FOR_LABEL_IS_DIFFERENT;
	
	public static String BUTTON_FIELD_EDITOR_BROWSE;

	public static String BUTTON_FIELD_EDITOR_NOT_IMPLEMENTED_YET;

	public static String COMPOSITE_EDITOR_EDITOR_SUPPORTS_ONLY_GRID_LAYOUT;

	public static String COMPOSITE_EDITOR_PARENT_CONTROL_SHOULD_BE_COMPOSITE;

	public static String COMPOSITE_EDITOR_THIS_METOD_CAN_BE_INVOKED;
	
	public static String TEXT_FIELD_EDITOR_EDITOR_SUPPORTS_ONLY_GRID_LAYOUT;

	public static String TEXT_FIELD_EDITOR_PARENT_CONTROL_SHOULD_BE_COMPOSITE;

	public static String BUTTON_ADD;

	public static String BUTTON_REMOVE;

	public static String BROWSER_COULD_NOT_OPEN_BROWSER;

	public static String BROWSER_COULD_NOT_DISPLAY_MALFORMED_URL;

	public static String URLSTRINGVALIDATOR_NOT_A_VALID_URL;
	
	public static String MANDATORYSTRING_VALIDATOR_MUST_PROVIDE_VALUE;
	
	public static String CONFIGURE_PROBLEM_SEVERITY;
	public static String ADD_SUPPRESS_WARNINGS_TITLE;
	public static String ADD_SUPPRESS_WARNINGS_MESSAGE;
	public static String ADD_SUPPRESS_WARNINGS_QUESTION1;
	public static String ADD_SUPPRESS_WARNINGS_QUESTION2;
	public static String ADD_SUPPRESS_WARNINGS_CANCEL;
	public static String ADD_SUPPRESS_WARNINGS_PROJECT;
	public static String ADD_SUPPRESS_WARNINGS_WORKSPACE;
	public static String ADD_SUPPRESS_WARNINGS_DISABLE;
	
	public static String NEW_SERVICE_WIZARD_TITLE;
	public static String NEW_SERVICE_WIZARD_INTERFACES_LABEL;
	public static String NEW_SERVICE_WIZARD_DESCRIPTION;
	public static String NEW_SERVICE_WIZARD_PAGE_NAME;
	public static String NEW_SERVICE_WIZARD_SERVICE_TYPE_EMPTY;
	public static String NEW_SERVICE_WIZARD_SERVICE_TYPE_NOT_EXISTS;
	public static String NEW_SERVICE_WIZARD_SERVICE_TYPE_FINAL;
	public static String NEW_SERVICE_WIZARD_SERVICE_TYPE_CONCRETE;
	public static String NEW_SERVICE_WIZARD_SELECT_SERVICE_TYPE_TITLE;

	public static String REGISTER_AS_SERVICE_TITLE;
	public static String REGISTER_AS_SERVICE_SUB_TITLE;
	public static String REGISTER_AS_SERVICE_MESSAGE;
	public static String REGISTER_AS_SERVICE_NO_TYPES_MESSAGE;
	public static String REGISTER_AS_SERVICE_TYPE_LABEL;
	public static String REGISTER_AS_SERVICE_NON_ABSTRACT_MESSAGE;
	public static String REGISTER_AS_SERVICE_ALREADY_REGISTERED_MESSAGE;
	public static String IMAGESBASE_URL_FOR_IMAGE_REGISTRY_CANNOT_BE_NULL;
	public static String IMAGESIMAGE_NAME_CANNOT_BE_NULL;

	static {
		NLS.initializeMessages(BUNDLE_NAME, CommonUIMessages.class);
	}
}