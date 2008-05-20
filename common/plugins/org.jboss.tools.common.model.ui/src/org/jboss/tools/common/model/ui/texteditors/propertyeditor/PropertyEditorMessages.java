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
package org.jboss.tools.common.model.ui.texteditors.propertyeditor;

import java.util.ResourceBundle;

/**
 * @author Jeremy
 *
 */
public class PropertyEditorMessages {

	private static final String RESOURCE_BUNDLE= "org.eclipse.ui.texteditor.ConstructedEditorMessages";//$NON-NLS-1$

	private static ResourceBundle fgResourceBundle= ResourceBundle.getBundle(RESOURCE_BUNDLE);

	private PropertyEditorMessages() {
	}

	public static ResourceBundle getResourceBundle() {
		return fgResourceBundle;
	}

}
