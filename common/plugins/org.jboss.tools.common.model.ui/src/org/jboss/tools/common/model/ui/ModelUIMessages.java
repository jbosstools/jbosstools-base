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
package org.jboss.tools.common.model.ui;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Aleksey
 */
public class ModelUIMessages {

	private static final String RESOURCE_BUNDLE= "org.jboss.tools.common.model.ui.messages";

	private static ResourceBundle resourceBundle= ResourceBundle.getBundle(RESOURCE_BUNDLE);

	private ModelUIMessages() {
	}

	public static String getString(String key) {
		try {
			return resourceBundle.getString(key);
		} catch (MissingResourceException e) {
			return "%" + key + "%";
		}
	}

	public static String getString(String key, String[] args) {
		return MessageFormat.format(getString(key), args);	
	}	
	
	public static ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

}
