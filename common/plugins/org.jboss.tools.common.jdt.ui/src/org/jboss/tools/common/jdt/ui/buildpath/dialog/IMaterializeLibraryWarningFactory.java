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
package org.jboss.tools.common.jdt.ui.buildpath.dialog;

import org.eclipse.jdt.core.IClasspathContainer;

/**
 * Warning message factory for classpath libraries to be materialized.
 * 
 * @author Fred Bricon
 *
 */
public interface IMaterializeLibraryWarningFactory {

	/**
	 * Returns a simple warning message associated with the materialization of a classpath library. 
	 * @return 	a warning message or <code>null</code> if it doesn't apply.
	 */
	String getWarning(IClasspathContainer classpathLibrary);
	
	/**
	 * Returns a warning message associated with the materialization of a classpath library. 
	 * This message is destined to be displayed in a dialog window.
	 * @return 	a warning message associated with the materialization of a classpath library, 
	 * 			or <code>null</code> if it doesn't apply.
	 */
	String getDialogWarning(IClasspathContainer classpathLibrary);
}
