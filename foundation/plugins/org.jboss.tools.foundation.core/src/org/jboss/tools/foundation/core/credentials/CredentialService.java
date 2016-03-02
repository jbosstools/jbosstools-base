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
package org.jboss.tools.foundation.core.credentials;

import org.jboss.tools.foundation.core.credentials.internal.CredentialsModel;

public class CredentialService {
	

	// Public domains that are first-class citizens of this framework and cannot be removed
	public static final String JBOSS_ORG = "jboss.org";
	public static final String REDHAT_ACCESS = "access.redhat.com";
	
	
	/**
	 * Our static instance
	 */
	private static ICredentialsModel model = CredentialsModel.getDefault();
	
	/**
	 * Public getter to access the service
	 * @return
	 */
	public static ICredentialsModel getCredentialModel() {
		return model;
	}
}
