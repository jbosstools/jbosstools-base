/*******************************************************************************
 * Copyright (c) 2020 Red Hat, Inc.
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
import java.net.InetSocketAddress;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.Deque;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.eclipse.swt.browser.Browser;
import org.keycloak.OAuth2Constants;
import org.keycloak.OAuthErrorException;
import org.keycloak.adapters.KeycloakDeployment;
import org.keycloak.adapters.ServerRequest;
import org.keycloak.adapters.rotation.AdapterTokenVerifier;
import org.keycloak.common.VerificationException;
import org.keycloak.common.util.Base64Url;
import org.keycloak.common.util.KeycloakUriBuilder;
import org.keycloak.common.util.RandomString;
import org.keycloak.representations.AccessToken;
import org.keycloak.representations.AccessTokenResponse;
import org.keycloak.representations.IDToken;

import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.handlers.AllowedMethodsHandler;
import io.undertow.server.handlers.GracefulShutdownHandler;
import io.undertow.server.handlers.PathHandler;
import io.undertow.util.Headers;
import io.undertow.util.Methods;
import io.undertow.util.StatusCodes;

/**
 * @author <a href="mailto:sthorger@redhat.com">Stian Thorgersen</a>
 */
public class KeycloakSWT {
  private KeycloakDeployment deployment;

  /**
   * local port to listen for callbacks. The value {@code 0} will choose a random
   * port.
   */
  private int listenPort = 0;

  /**
   * local hostname to listen for callbacks.
   */
  private String listenHostname = "localhost";

  private AccessTokenResponse tokenResponse;
  private String tokenString;
  private String idTokenString;
  private IDToken idToken;
  private AccessToken token;
  private String refreshToken;
  private Locale locale;
  Pattern callbackPattern = Pattern.compile("callback\\s*=\\s*\"([^\"]+)\"");
  Pattern paramPattern = Pattern.compile("param=\"([^\"]+)\"\\s+label=\"([^\"]+)\"\\s+mask=(\\S+)");
  Pattern codePattern = Pattern.compile("code=([^&]+)");

  private CallbackListener callback;

  public KeycloakSWT(KeycloakDeployment deployment) {
    this.deployment = deployment;
  }

  public Locale getLocale() {
    return locale;
  }

  public void setLocale(Locale locale) {
    this.locale = locale;
  }

  public int getListenPort() {
    return listenPort;
  }

  /**
   * Configures the local port to listen for callbacks. The value {@code 0} will
   * choose a random port. Defaults to {@code 0}.
   * 
   * @param listenPort a valid port number
   */
  public void setListenPort(int listenPort) {
    if (listenPort < 0 || listenPort > 65535) {
      throw new IllegalArgumentException("localPort");
    }
    this.listenPort = listenPort;
  }

  public String getListenHostname() {
    return listenHostname;
  }

  /**
   * Configures the local hostname to listen for callbacks. The value {@code 0}
   * will choose a random port
   * 
   * @param listenHostname a valid local hostname
   */
  public void setListenHostname(String listenHostname) {
    this.listenHostname = listenHostname;
  }

  public void logout(Browser browser) throws IOException, InterruptedException, URISyntaxException {
    logoutDesktop(browser);

    tokenString = null;
    token = null;

    idTokenString = null;
    idToken = null;

    refreshToken = null;

  }

  public void loginDesktop(Browser browser) throws IOException, VerificationException, OAuthErrorException,
      URISyntaxException, ServerRequest.HttpFailure, InterruptedException {
    callback = new CallbackListener();
    callback.start();

    String redirectUri = String.format("http://%s:%s", getListenHostname(), callback.getLocalPort());
    String state = UUID.randomUUID().toString();
    Pkce pkce = deployment.isPkce() ? generatePkce() : null;

    String authUrl = createAuthUrl(redirectUri, state, pkce);

    browser.getDisplay().asyncExec(() -> browser.setUrl(authUrl));

    try {
      callback.await();
    } catch (InterruptedException e) {
      callback.stop();
      throw e;
    }

    if (callback.error != null) {
      throw new OAuthErrorException(callback.error, callback.errorDescription);
    }

    if (!state.equals(callback.state)) {
      throw new VerificationException("Invalid state");
    }

    processCode(callback.code, redirectUri, pkce);
  }

  protected String createAuthUrl(String redirectUri, String state, Pkce pkce) {

    KeycloakUriBuilder builder = deployment.getAuthUrl().clone()
        .queryParam(OAuth2Constants.RESPONSE_TYPE, OAuth2Constants.CODE)
        .queryParam(OAuth2Constants.CLIENT_ID, deployment.getResourceName())
        .queryParam(OAuth2Constants.REDIRECT_URI, redirectUri)
        .queryParam(OAuth2Constants.SCOPE, OAuth2Constants.SCOPE_OPENID);

    if (state != null) {
      builder.queryParam(OAuth2Constants.STATE, state);
    }

    if (locale != null) {
      builder.queryParam(OAuth2Constants.UI_LOCALES_PARAM, locale.getLanguage());
    }

    if (pkce != null) {
      builder.queryParam(OAuth2Constants.CODE_CHALLENGE, pkce.getCodeChallenge());
      builder.queryParam(OAuth2Constants.CODE_CHALLENGE_METHOD, "S256");
    }

    return builder.build().toString();
  }

  protected Pkce generatePkce() {
    return Pkce.generatePkce();
  }

  private void logoutDesktop(Browser browser) throws IOException, URISyntaxException, InterruptedException {
    CallbackListener callback = new CallbackListener();
    callback.start();

    String redirectUri = String.format("http://%s:%s", getListenHostname(), callback.getLocalPort());

    String logoutUrl = deployment.getLogoutUrl().clone().queryParam(OAuth2Constants.REDIRECT_URI, redirectUri).build()
        .toString();

    browser.getDisplay().asyncExec(() -> browser.setUrl(logoutUrl));

    try {
      callback.await();
    } catch (InterruptedException e) {
      callback.stop();
      throw e;
    }
  }

  public String getTokenString() {
    return tokenString;
  }

  public String getTokenString(long minValidity, TimeUnit unit)
      throws VerificationException, IOException, ServerRequest.HttpFailure {
    long expires = ((long) token.getExpiration()) * 1000 - unit.toMillis(minValidity);
    if (expires < System.currentTimeMillis()) {
      refreshToken();
    }

    return tokenString;
  }

  public void refreshToken() throws IOException, ServerRequest.HttpFailure, VerificationException {
    AccessTokenResponse tokenResponse = ServerRequest.invokeRefresh(deployment, refreshToken);
    parseAccessToken(tokenResponse);
  }

  public void refreshToken(String refreshToken) throws IOException, ServerRequest.HttpFailure, VerificationException {
    AccessTokenResponse tokenResponse = ServerRequest.invokeRefresh(deployment, refreshToken);
    parseAccessToken(tokenResponse);

  }

  private void parseAccessToken(AccessTokenResponse tokenResponse) throws VerificationException {
    this.tokenResponse = tokenResponse;
    tokenString = tokenResponse.getToken();
    refreshToken = tokenResponse.getRefreshToken();
    idTokenString = tokenResponse.getIdToken();

    AdapterTokenVerifier.VerifiedTokens tokens = AdapterTokenVerifier.verifyTokens(tokenString, idTokenString,
        deployment);
    token = tokens.getAccessToken();
    idToken = tokens.getIdToken();
  }

  public AccessToken getToken() {
    return token;
  }

  public IDToken getIdToken() {
    return idToken;
  }

  public String getIdTokenString() {
    return idTokenString;
  }

  public String getRefreshToken() {
    return refreshToken;
  }

  public AccessTokenResponse getTokenResponse() {
    return tokenResponse;
  }

  public KeycloakDeployment getDeployment() {
    return deployment;
  }

  private void processCode(String code, String redirectUri, Pkce pkce)
      throws IOException, ServerRequest.HttpFailure, VerificationException {

    AccessTokenResponse tokenResponse = ServerRequest.invokeAccessCodeToToken(deployment, code, redirectUri, null,
        pkce == null ? null : pkce.getCodeVerifier());
    parseAccessToken(tokenResponse);
  }

  class CallbackListener implements HttpHandler {
    private final CountDownLatch shutdownSignal = new CountDownLatch(1);

    private String code;
    private String error;
    private String errorDescription;
    private String state;
    private Undertow server;

    private GracefulShutdownHandler gracefulShutdownHandler;

    public void start() {
      PathHandler pathHandler = Handlers.path().addExactPath("/", this);
      AllowedMethodsHandler allowedMethodsHandler = new AllowedMethodsHandler(pathHandler, Methods.GET);
      gracefulShutdownHandler = Handlers.gracefulShutdown(allowedMethodsHandler);

      server = Undertow.builder().setIoThreads(1).setWorkerThreads(1)
          .addHttpListener(getListenPort(), getListenHostname()).setHandler(gracefulShutdownHandler).build();

      server.start();
    }

    public void stop() {
      try {
        server.stop();
      } catch (Exception ignore) {
        // it is OK to happen if thread is modified while stopping the server, specially
        // when a security manager is enabled
      }
      shutdownSignal.countDown();
    }

    public int getLocalPort() {
      return ((InetSocketAddress) server.getListenerInfo().get(0).getAddress()).getPort();
    }

    public void await() throws InterruptedException {
      shutdownSignal.await();
    }

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
      gracefulShutdownHandler.shutdown();

      if (!exchange.getQueryParameters().isEmpty()) {
        readQueryParameters(exchange);
      }

      exchange.setStatusCode(StatusCodes.FOUND);
      exchange.getResponseHeaders().add(Headers.LOCATION, getRedirectUrl());
      exchange.endExchange();

      shutdownSignal.countDown();

      ForkJoinPool.commonPool().execute(this::stop);
    }

    private void readQueryParameters(HttpServerExchange exchange) {
      code = getQueryParameterIfPresent(exchange, OAuth2Constants.CODE);
      error = getQueryParameterIfPresent(exchange, OAuth2Constants.ERROR);
      errorDescription = getQueryParameterIfPresent(exchange, OAuth2Constants.ERROR_DESCRIPTION);
      state = getQueryParameterIfPresent(exchange, OAuth2Constants.STATE);
    }

    private String getQueryParameterIfPresent(HttpServerExchange exchange, String name) {
      Map<String, Deque<String>> queryParameters = exchange.getQueryParameters();
      return queryParameters.containsKey(name) ? queryParameters.get(name).getFirst() : null;
    }

    private String getRedirectUrl() {
      String redirectUrl = deployment.getTokenUrl().replace("/token", "/delegated");

      if (error != null) {
        redirectUrl += "?error=true";
      }

      return redirectUrl;
    }
  }

  public static class Pkce {
    // https://tools.ietf.org/html/rfc7636#section-4.1
    public static final int PKCE_CODE_VERIFIER_MAX_LENGTH = 128;

    private final String codeChallenge;
    private final String codeVerifier;

    public Pkce(String codeVerifier, String codeChallenge) {
      this.codeChallenge = codeChallenge;
      this.codeVerifier = codeVerifier;
    }

    public String getCodeChallenge() {
      return codeChallenge;
    }

    public String getCodeVerifier() {
      return codeVerifier;
    }

    public static Pkce generatePkce() {
      try {
        String codeVerifier = new RandomString(PKCE_CODE_VERIFIER_MAX_LENGTH, new SecureRandom()).nextString();
        String codeChallenge = generateS256CodeChallenge(codeVerifier);
        return new Pkce(codeVerifier, codeChallenge);
      } catch (Exception ex) {
        throw new RuntimeException("Could not generate PKCE", ex);
      }
    }

    // https://tools.ietf.org/html/rfc7636#section-4.6
    private static String generateS256CodeChallenge(String codeVerifier) throws Exception {
      MessageDigest md = MessageDigest.getInstance("SHA-256");
      md.update(codeVerifier.getBytes(StandardCharsets.ISO_8859_1));
      return Base64Url.encode(md.digest());
    }
  }

  /**
   * 
   */
  public void close() {
    if (callback != null) {
      callback.stop();
    }
  }
}
