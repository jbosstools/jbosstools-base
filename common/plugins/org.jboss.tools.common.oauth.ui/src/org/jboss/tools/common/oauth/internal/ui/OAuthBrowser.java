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
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
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
    allowNonHttpsRedirects();
    browser = new Browser(this, SWT.NONE);
    browser.setLayoutData(new GridData( GridData.FILL_BOTH));
  }

  /**
   * Set the transport policy on MacOS to allow redirects to non-https urls.
   * 
   * @see <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=568749">Eclipse bug 568749</a>
   */
  private void allowNonHttpsRedirects() {
	if (!Platform.OS_MACOSX.equals(Platform.getOS())) {
		return;
	}

	try {
		/*
		 * 	NSDictionary allowNonHttps = NSDictionary.dictionaryWithObject(
		 *	            NSNumber.numberWithBool(true),
		 *	            NSString.stringWith("NSAllowsArbitraryLoads"));
		 */
		Class<?> nsNumberClass = Class.forName("org.eclipse.swt.internal.cocoa.NSNumber");
		Method numberWithBoolMethod = nsNumberClass.getDeclaredMethod("numberWithBool", Boolean.TYPE);
		Class<?> nsStringClass = Class.forName("org.eclipse.swt.internal.cocoa.NSString");
		Method stringWithMethod = nsStringClass.getDeclaredMethod("stringWith", String.class);
		Class<?> nsDictionnaryClass = Class.forName("org.eclipse.swt.internal.cocoa.NSDictionary");
		Class<?> idClass = Class.forName("org.eclipse.swt.internal.cocoa.id");
		Method dictionnaryWithObjectMethod = nsDictionnaryClass.getMethod("dictionaryWithObject", idClass, idClass);
		Object allowNonHttps = dictionnaryWithObjectMethod.invoke(dictionnaryWithObjectMethod,
				numberWithBoolMethod.invoke(nsNumberClass, true),
				stringWithMethod.invoke(nsStringClass, "NSAllowsArbitraryLoads"));

		/*
		 * 	    NSBundle.mainBundle().infoDictionary().setValue(
	     *       	allowNonHttps, NSString.stringWith("NSAppTransportSecurity"));
		 */
		Class<?> nsBundle = Class.forName("org.eclipse.swt.internal.cocoa.NSBundle");
		Object mainBundle = nsBundle.getDeclaredMethod("mainBundle").invoke(nsBundle);
		Object infoDictionary = mainBundle.getClass().getDeclaredMethod("infoDictionary").invoke(mainBundle);
		Method setValue = infoDictionary.getClass().getMethod("setValue", idClass, nsStringClass);
		setValue.invoke(infoDictionary,
				allowNonHttps,
				stringWithMethod.invoke(stringWithMethod, "NSAppTransportSecurity"));
	} catch (ReflectiveOperationException | SecurityException | IllegalArgumentException e) {
		CommonOAuthUIActivator.logError("Could not allow non-https redirects.", e);
	}
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
