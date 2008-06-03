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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class EditorMessages {

	private static final String RESOURCE_BUNDLE= "org.jboss.tools.common.model.ui.attribute.editor.messages";//$NON-NLS-1$

	private static ResourceBundle bundle= ResourceBundle.getBundle(RESOURCE_BUNDLE);

	private EditorMessages() {
	}

	public static String getString(String key) {
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}
}


