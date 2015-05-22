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

import org.eclipse.osgi.util.NLS;

public class JVMProblemDetectorMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.foundation.checkup.JVMProblemDetectorMessages"; //$NON-NLS-1$

	static {
		NLS.initializeMessages(BUNDLE_NAME, JVMProblemDetectorMessages.class);
	}
	
	public static String JOB_TITLE;
	public static String SHOW_WARNING_DIALOG_JOB_TITLE;
	public static String UNRESOLVED_METHOD_LABEL;
	public static String DEPENDANT_MODULES;
	public static String WARNING_DIALOG_MESSAGE;
	public static String WARNING_DIALOG_ADVISE;
	public static String DO_NOT_SHOW;
}
