/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.checkup;

import org.eclipse.core.runtime.Status;

public class TestEnvironmentDetector {
	// possible values of eclipse.application system property
	// need to cover:
	// "org.eclipse.pde.junit.runtime.uitestapplication"
	// "org.eclipse.pde.junit.runtime.coretestapplication"
	private static final String JUNIT_APPLICATION_PREFIX = "org.eclipse.pde.junit.runtime."; //$NON-NLS-1$
	
	// need to cover
	// "org.eclipse.tycho.surefire.osgibooter.uitest32"
	// "org.eclipse.tycho.surefire.osgibooter.uitest"
	// "org.eclipse.tycho.surefire.osgibooter.headlesstest"
	private static final String TYCHO_APPLICATION_PREFIX = "org.eclipse.tycho.surefire.osgibooter."; //$NON-NLS-1$

	/**
	 * Returns true if application launched from the test environment
	 * @return
	 */
	public static boolean isTestEnvironment(){
		String eclipseApplication = System.getProperty("eclipse.application"); //$NON-NLS-1$
		if(eclipseApplication != null && (eclipseApplication.startsWith(JUNIT_APPLICATION_PREFIX) ||
				eclipseApplication.startsWith(TYCHO_APPLICATION_PREFIX))){
			
			// log error in case of wrong detection
			FoundationCheckupPlugin.getDefault().getLog()
					.log(new Status(Status.ERROR, FoundationCheckupPlugin.PLUGIN_ID,
							"Test environment detected. See org.jboss.tools.foundation.checkup.TestEnvironmentDetector. eclipse.application - "
									+ eclipseApplication));
			return true;
		}
		return false;
	}

}
