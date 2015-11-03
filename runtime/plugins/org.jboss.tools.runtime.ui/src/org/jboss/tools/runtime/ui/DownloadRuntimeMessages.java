/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.runtime.ui;

import org.eclipse.osgi.util.NLS;

public class DownloadRuntimeMessages {
	
	private static String BUNDLE_NAME = DownloadRuntimeMessages.class.getName();
	
	public static String CredentialsFragmentTitle;
	public static String CredentialsFragmentDescription;
	public static String CredentialsFragmentInstructions;
	public static String ValidatingCredentials;
	public static String CredentialsIncorrect;
	public static String CredentialWorkflowFailed;
	public static String CredentialError;
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, DownloadRuntimeMessages.class);
	}
}
