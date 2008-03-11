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
package org.jboss.tools.common.text.xml.ui;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * 
 * @author Jeremy
 *
 */
public class TextEditorMessages {
	private static final String RESOURCE_BUNDLE= "org.jboss.tools.common.text.xml.ui.TextEditorMessages";//$NON-NLS-1$
	private static ResourceBundle fResourceBundle= ResourceBundle.getBundle(RESOURCE_BUNDLE);

	private TextEditorMessages() {}

	/**
	 * 
	 * @param key
	 * @return
	 */
	public static String getString(String key) {
		try {
			return fResourceBundle.getString(key); 
		} catch (MissingResourceException e) {
			return '!' + key + '!';
		}
	}

	/**
	 * 
	 * @return
	 */
	public static ResourceBundle getResourceBundle() {
		return fResourceBundle;
	}

	/**
	 * Gets a formatted with arguments string from the resource bundle
	 */
	public static String getFormattedString(String key, Object[] args) {
		return MessageFormat.format(getString(key), args);
	}

	/**
	 * Gets a formatted with arguments string from the resource bundle
	 */
	public static String getFormattedString(String key, Object arg) {
		return MessageFormat.format(getString(key), new Object[] { arg } );
	}
}
