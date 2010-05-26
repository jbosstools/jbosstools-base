package org.jboss.tools.test.util;

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.tests.TestsPlugin;

public class Debug {
	public static final boolean DEBUG_IMPORT_OPERATION;
	
	static {
		DEBUG_IMPORT_OPERATION = true; //getDebugOption("/debug/importProject");
	}
	
    private static boolean getDebugOption(String option) {
        return "true".equalsIgnoreCase(Platform.getDebugOption(TestsPlugin.ID + option)); //$NON-NLS-1$
    }
}
