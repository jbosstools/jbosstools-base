/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.gef.outline.xpl;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.eclipse.osgi.util.NLS;

public class GEFUIMessages extends NLS {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.gef.outline.xpl.messages";//$NON-NLS-1$
	private static ResourceBundle fResourceBundle;
	static {
		// load message values from bundle file
		NLS.initializeMessages(BUNDLE_NAME, GEFUIMessages.class);
	}

	private GEFUIMessages() {
		// cannot create new instance of this class
	}

	public static ResourceBundle getResourceBundle() {
		try {
			if (fResourceBundle == null)
				fResourceBundle = ResourceBundle.getBundle(BUNDLE_NAME);
		}
		catch (MissingResourceException x) {
			fResourceBundle = null;
		}
		return fResourceBundle;
	}

	public static String TREE;
	public static String SHOW_TREE;
	public static String DIAGRAM_NAVIGATOR;
	public static String SHOW_DIAGRAM_NAVIGATOR;
}