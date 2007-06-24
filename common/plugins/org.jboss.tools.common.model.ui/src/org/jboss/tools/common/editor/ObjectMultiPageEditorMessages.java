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

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Jeremy
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class ObjectMultiPageEditorMessages {
	private static ResourceBundle bundle = ResourceBundle
		.getBundle("org.jboss.tools.common.editor.ObjectMultiPageEditorMessages"); //$NON-NLS-1$

	private ObjectMultiPageEditorMessages() {}

	public static String getString( String key ) {
		try {
			return bundle.getString( key );
		} catch ( MissingResourceException e ) {
			return "!!!" + key + "!!!"; //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

}
