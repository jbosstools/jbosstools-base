/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.internal.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.handlers.HandlerUtil;
import org.jboss.tools.common.oauth.core.CommonOAuthCoreConstants;
import org.jboss.tools.common.oauth.core.TokenProvider;
import org.jboss.tools.common.oauth.internal.ui.CommonOAuthUIActivator;

public class GetTokenHandler extends AbstractHandler {
  
  private static final String TITLE = "SSO Login";

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			TokenProvider provider = TokenProvider.get();
			String token = provider.getToken(CommonOAuthCoreConstants.REDHAT_SSO_SERVER_ID);
			MessageDialog.openInformation(HandlerUtil.getActiveShell(event), TITLE,
					"Token retrieved is:" + token.substring(0, 16));
		} catch (Exception e) {
		  e.printStackTrace();
			IStatus status = new Status(IStatus.ERROR, CommonOAuthUIActivator.PLUGIN_ID, e.getLocalizedMessage(), e);
			ErrorDialog dialog = new ErrorDialog(HandlerUtil.getActiveShell(event), TITLE,
					e.getLocalizedMessage(), status, IStatus.ERROR);
			dialog.open();
		}
		return null;
	}
}
