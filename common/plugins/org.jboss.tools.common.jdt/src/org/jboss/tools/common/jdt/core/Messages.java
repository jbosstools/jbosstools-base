/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.core;

import org.eclipse.osgi.util.NLS;

/**
 * 
 * @author Fred Bricon
 *
 */
public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.jdt.core.messages"; //$NON-NLS-1$
	
	public static String Materialize_Library;

	public static String MaterializeLibraryJob_error_copying_file;

	public static String MaterializeLibraryJob_Error_creating_classpath_entry;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
