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
package org.jboss.tools.common.model.ui.messages;

import org.eclipse.osgi.util.NLS;


public class UIMessages extends NLS {
	
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.messages.messages";//$NON-NLS-1$
	static {
		// load message values from bundle file
		NLS.initializeMessages(BUNDLE_NAME, UIMessages.class);		
	}
	public static String ERROR_CREATING_NESTED_TEXT_EDITOR;
	public static String CHANGE_FONT;
	public static String PROPERTIES;
	public static String PREVIEW;
	public static String MUST_BE_IFILEEDITORINPUT;
}
