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


	static {
		NLS.initializeMessages(BUNDLE_NAME, CommonUIMessages.class);
	}
}