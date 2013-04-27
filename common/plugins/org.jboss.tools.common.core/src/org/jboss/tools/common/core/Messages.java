/******************************************************************************* 
 * Copyright (c) 2009 - 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

package org.jboss.tools.common.core;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.core.messages"; //$NON-NLS-1$
	public static String SAXValidator_IOExceptionMessage;
	public static String SAXValidator_SAXExceptionMessage;
	public static String SAXValidator_UnableToInstantiateMessage;
	public static String XMLUtilities_IOExceptionMessage;
	public static String XMLUtilities_SAXExceptionMessage;
	public static String AbstractClasspathContainer_error_loading_container;
	public static String ClasspathDecorationsManager_unexpected_exception;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
