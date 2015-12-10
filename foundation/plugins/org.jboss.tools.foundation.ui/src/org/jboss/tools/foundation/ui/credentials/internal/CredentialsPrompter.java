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
package org.jboss.tools.foundation.ui.credentials.internal;

import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.core.credentials.ICredentialsPrompter;

public class CredentialsPrompter implements ICredentialsPrompter {

	public CredentialsPrompter() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public String getPassword(ICredentialDomain domain, String user) {

		final CredentialPromptDialog loginDialog = new CredentialPromptDialog(domain, user);
		final String[] result = new String[1];
		PlatformUI.getWorkbench().getDisplay().syncExec(new Runnable() {
			public void run() {
				if (loginDialog.open() == Window.OK)
					result[0] = loginDialog.getPassword();
				else
					result[0] = null;
			}
		});
		return result[0];
	}

}
