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
package org.jboss.tools.common.editor;

import org.eclipse.osgi.util.NLS;

/**
 * @author Jeremy
 */
public class ObjectMultiPageEditorMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.editor.ObjectMultiPageEditorMessages"; //$NON-NLS-1$

	static {
		// load message values from bundle file
		NLS.initializeMessages(BUNDLE_NAME, ObjectMultiPageEditorMessages.class);
	}

	public static String PAGES_EDITOR_SOURCE_TAB;

}