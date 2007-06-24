/*******************************************************************************
 * Copyright (c) 2001, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jens Lukowski/Innoopract - initial renaming/restructuring
 *     Exadel, Inc.
 *     Red Hat, Inc.     
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import org.eclipse.osgi.util.NLS;

public class Messages {

	
	private Messages() { }
	
	static {
		NLS.initializeMessages(
				"org.jboss.tools.common.text.ext.hyperlink.xpl.Messages", 
				Messages.class);
	}
	
	public static String cannotOpenLink; 
}
