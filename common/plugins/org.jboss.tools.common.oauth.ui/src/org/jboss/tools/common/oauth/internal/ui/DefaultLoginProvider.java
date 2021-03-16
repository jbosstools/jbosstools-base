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
package org.jboss.tools.common.oauth.internal.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.common.oauth.core.LoginProvider;
import org.jboss.tools.common.oauth.core.LoginResponse;
import org.jboss.tools.common.oauth.core.OAuthUtils;
import org.jboss.tools.common.oauth.core.model.IAccount;
import org.jboss.tools.common.oauth.core.model.IAuthorizationServer;
import org.keycloak.adapters.KeycloakDeployment;

/**
 * Login provider that will launch a browser to perform the login and extract
 * the JSON.
 * 
 */
public class DefaultLoginProvider implements LoginProvider {

	private static final int TIMEOUT_JOB_ON_UI_THREAD = 10000;

	private class LoginJob extends UIJob {
		private boolean runninginUI = false;
		private boolean shouldRun = true;
		private IAuthorizationServer server;
		private IAccount account;
		private LoginResponse response;

		public LoginJob(IAuthorizationServer server, IAccount account) {
			super("Logging to " + server.getURL());
			this.server = server;
			this.account = account;
		}

		@Override
		public IStatus runInUIThread(IProgressMonitor monitor) {
			if (shouldRun && !monitor.isCanceled()) {
				runninginUI = true;
				response = loginInUI(server, account);
				return new Status(IStatus.OK, CommonOAuthUIActivator.PLUGIN_ID, null);
			}
			return new Status(IStatus.CANCEL, CommonOAuthUIActivator.PLUGIN_ID, null);
		}

		public boolean isRunninginUI() {
			return runninginUI;
		}

		public void setShouldRun(boolean shouldRun) {
			this.shouldRun = shouldRun;
		}

		public LoginResponse getResponse() {
			return response;
		}
	}

	@Override
	public LoginResponse login(IAuthorizationServer server, IAccount account, Object context) {
		if (null == Display.getCurrent()) {
			return runInJob(server, account);
		} else {
			return loginInUI(server, account);
		}
	}

	LoginResponse runInJob(IAuthorizationServer server, IAccount account) {
		LoginJob job = new LoginJob(server, account);
		job.schedule();
		IProgressMonitor monitor = new NullProgressMonitor();
		try {
			if (job.join(TIMEOUT_JOB_ON_UI_THREAD, monitor)) {
				return job.getResponse();
			} else {
				throw new InterruptedException();
			}
		} catch (OperationCanceledException e) {
			return null;
		} catch (InterruptedException e) {
		  Thread.currentThread().interrupt();
			if (job.isRunninginUI()) {
				try {
					job.join();
					return job.getResponse();
				} catch (InterruptedException e1) {
				  Thread.currentThread().interrupt();
					return null;
				}
			} else {
				job.setShouldRun(false);
				monitor.setCanceled(true);
				job.cancel();
				return null;
			}
		}
	}

	public LoginResponse loginInUI(IAuthorizationServer server, IAccount account) {
		LoginResponse response = null;
		
		KeycloakDeployment deployment = OAuthUtils.getDeployment(server);

		BrowserBasedLoginDialog dialog = new BrowserBasedLoginDialog(Display.getCurrent().getActiveShell(),
				deployment);
		if (dialog.open() == Window.OK) {
			response = dialog.getInfo();
		}
		return response;
	}
}
