/******************************************************************************* 
 * Copyright (c) 2007-2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.test.util;

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.tests.TestsPlugin;

public class Debug {
	public static final boolean DEBUG_IMPORT_OPERATION;
	
	static {
		DEBUG_IMPORT_OPERATION = getDebugOption("/debug/importProject");
	}
	
    private static boolean getDebugOption(String option) {
        return "true".equalsIgnoreCase(Platform.getDebugOption(TestsPlugin.ID + option)); //$NON-NLS-1$
    }
}
