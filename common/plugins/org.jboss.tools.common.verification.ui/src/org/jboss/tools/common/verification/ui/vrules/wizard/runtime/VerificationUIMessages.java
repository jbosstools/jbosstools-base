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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import org.eclipse.osgi.util.NLS;

public class VerificationUIMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.verification.ui.vrules.wizard.runtime.messages";//$NON-NLS-1$
	static {
		// load message values from bundle file
		NLS.initializeMessages(BUNDLE_NAME, VerificationUIMessages.class);		
	}
	
	public static String WARNING;
	public static String LIMIT_OF_REPORTED_ERRORS_IS_REACHED;
	public static String OK;
	public static String VerifyWizardView_Close;
	public static String VerifyWizardView_Pause;
	public static String VerifyWizardView_Resume;
	public static String VerifyWizardView_RunAll;
	public static String VerifyWizardView_RunSelected;
	public static String VerifyWizardView_Stop;
}
