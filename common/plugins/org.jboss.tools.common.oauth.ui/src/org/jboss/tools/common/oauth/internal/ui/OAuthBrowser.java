/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.internal.ui;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.common.oauth.core.LoginResponse;
import org.keycloak.OAuthErrorException;
import org.keycloak.adapters.KeycloakDeployment;
import org.keycloak.adapters.ServerRequest.HttpFailure;
import org.keycloak.common.VerificationException;
import org.keycloak.representations.AccessTokenResponse;

/**
 * @author Red Hat Developers
 *
 */
public class OAuthBrowser extends Composite implements DisposeListener {
  private final Browser browser;
  private KeycloakSWT adapter;
  private CompletableFuture<LoginResponse> redirectFuture = new CompletableFuture<>();

  /**
   * @param parent
   * @param style
   */
  public OAuthBrowser(Composite parent, int style) {
    super(parent, style);
    addDisposeListener(this);
    setLayout(new GridLayout(1, false));
    setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true, true));
    browser = new Browser(this, SWT.NONE);
    browser.setLayoutData(new GridData( GridData.FILL_BOTH));
  }
  
  private IStatus processRedirect(IProgressMonitor monitor) {
    try {
      adapter.loginDesktop(browser);
      AccessTokenResponse response = adapter.getTokenResponse();
      LoginResponse info = new LoginResponse();
      info.setIDToken(response.getIdToken());
      info.setAccessToken(response.getToken());
      info.setAccessTokenExpiryTime(System.currentTimeMillis() + response.getExpiresIn() * 1000);
      info.setRefreshToken(response.getRefreshToken());
      info.setRefreshTokenExpiryTime(System.currentTimeMillis() + response.getRefreshExpiresIn() * 1000);
      redirectFuture.complete(info);
    } catch (IOException | VerificationException | OAuthErrorException | URISyntaxException | HttpFailure
        | InterruptedException e) {
      redirectFuture.completeExceptionally(e);
    }
    return Status.OK_STATUS;
  }
  
  public void setDeployment(KeycloakDeployment deployment) {
    adapter = new KeycloakSWT(deployment);
    Job.create("Process OAuth redirect", this::processRedirect).schedule();
  }

  @Override
  public void widgetDisposed(DisposeEvent e) {
    if (adapter != null) {
      adapter.close();
    }
  }
  
  public CompletableFuture<LoginResponse> getRedirectFuture() {
    return redirectFuture;
  }

  /**
   * @param text the text to display
   */
  public void setText(String text) {
    browser.setText(text);
  }
}
