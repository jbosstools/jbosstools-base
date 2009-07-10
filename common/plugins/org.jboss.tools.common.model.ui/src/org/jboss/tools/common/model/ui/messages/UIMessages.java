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
	public static String CANNOT_DISPLAY_DOCUMENT_CONTENTS;
	public static String PROPERTIES_EDITOR_FILTER;
	public static String PROPERTIES_EDITOR_FILTER_MATCHES;
	public static String PROPERTIES_EDITOR_EXPRESSION;
	public static String PROPERTIES_EDITOR_ILLEGAL_NAME_EXPRESSION;
	public static String PROPERTIES_EDITOR_ILLEGAL_VALUE_EXPRESSION;
}
