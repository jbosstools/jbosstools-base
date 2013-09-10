/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime.ui.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;

public class AuthenticatorUIWrapper {
	private static final String PROP_AUTH_ID = "authenticatorId"; //$NON-NLS-1$
	private static final String PROP_CLASS = "class"; //$NON-NLS-1$


	private IConfigurationElement element;
	private String authId;
	
	public AuthenticatorUIWrapper(IConfigurationElement ce) {
		this.element = ce;
		this.authId = ce.getAttribute(PROP_AUTH_ID);
	}
	
	public String getAuthenticatorId() {
		return authId;
	}
	
	public WizardFragment createWizardFragment() throws CoreException {
		return (WizardFragment) element.createExecutableExtension(PROP_CLASS);
	}
}
