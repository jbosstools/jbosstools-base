/*
 * Copyright 2004-2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shale.usecases.profile;

import java.util.Map;
import javax.faces.context.FacesContext;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.usecases.logic.LogonLogic;
import org.apache.shale.usecases.model.User;
import org.apache.shale.util.Messages;
import org.apache.shale.view.AbstractViewController;

/**
 * <p><code>ViewController</code> and action methods for the Logon dialog.</p>
 *
 * <p><strong>WARNING</strong> - The format of the cookie used to store
 * "remember me" credentials is <strong>NOT</strong> secure, and should
 * be considered demo quality.  The architecture of a production quality
 * version of this function would be identical; more effort would need
 * to be invested in improving security around the cookie values.</p>
 *
 * $Id: LogonActions.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class LogonActions extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>Log instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(LogonActions.class);


    /**
     * <p>Message resources for this application.</p>
     */
    private static final Messages messages =
      new Messages("org.apache.shale.usecases.view.Bundle");


    // ------------------------------------------------------ Manifest Constants



    /**
     * <p>Logical outcome indicating an authenticated user.</p>
     */
    static final String AUTHENTICATED = "authenticated";


    /**
     * <p>Logical outcome indicating the user wishes to create a new
     * user profile.</p>
     */
    static final String CREATE = "create";


    /**
     * <p>Managed bean name under which the business logic bean instance
     * for this dialog is stored.</p>
     */
    static final String LOGIC_BEAN = "profile$logic"; // FIXME - shared name


    /**
     * <p>Name of the HTTP cookie in which we store "remember me" credentials.</p>
     */
    static final String COOKIE_NAME = "remember_me";


    /**
     * <p>Logical outcome indicating an unauthenticated user.</p>
     */
    static final String UNAUTHENTICATED = "unauthenticated";


    // --------------------------------------------------- Configured Properties


    /**
     * <p>Flag indicating that "remember me" cookies are enabled.</p>
     */
    private boolean rememberMe = false;
    public boolean isRememberMe() { return this.rememberMe; }
    public void setRememberMe(boolean rememberMe) { this.rememberMe = rememberMe; }


    /**
     * <p>Session scope attribute under which a {@link User} instance for the
     * currently logged in user is stored.</p>
     */
    private String userKey = "user"; // FIXME - shared
    public String getUserKey() { return this.userKey; }
    public void setUserKey(String userKey) { this.userKey = userKey; }


    // -------------------------------------------------- Input Field Properties


    /**
     * <p>Password entered by the user.</p>
     */
    private String password = null;
    public String getPassword() { return this.password; }
    public void setPassword(String password) { this.password = password; }


    /**
     * <p>Flag indicating that the user wishes to have a "remember me"
     * cookie created this time.</p>
     */
    private boolean remember = false;
    public boolean isRemember() { return this.remember; }
    public void setRemember(boolean remember) { this.remember = remember; }


    /**
     * <p>Username entered by the user.</p>
     */
    private String username = null;
    public String getUsername() { return this.username; }
    public void setUsername(String username) { this.username = username; }


    // ----------------------------------------------------------------- Actions


    /**
     * <p>Skip the logon dialog if an appropriate "remember me" cookie
     * is discovered, and this facility is enabled.</p>
     *
     * <p>The following logical outcome values are returned:</p>
     * <ul>
     * <li><code>AUTHENTICATED</code> - User has been authenticated.</li>
     * <li><code>UNAUTHENTICATED</code> - User has not been authenticated
     *     (no cookie, "remember me" not supported).</li>
     * </ul>
     */
    public String check() {

        // Is "remember me" functionality enabled?
        if (!isRememberMe()) {
            return UNAUTHENTICATED;
        }

        // Locate the "remember me" cookie (if any)
        FacesContext context = getFacesContext();
        Map map = context.getExternalContext().getRequestCookieMap();
        Cookie cookie = (Cookie) map.get(COOKIE_NAME);
        if (cookie == null) {
            return UNAUTHENTICATED;
        }

        // Extract the user identifier of the logged-on user (if any)
        int id = 0;
        try {
            id = Integer.parseInt(cookie.getValue());
        } catch (NumberFormatException e) {
            return UNAUTHENTICATED;
        }

        // Locate the corresponding valid user (if any) and return it
        LogonLogic logic = (LogonLogic) getBean(LOGIC_BEAN);
        User user = logic.findUser(id);
        if (user == null) {
            return UNAUTHENTICATED;
        }

        // Register the newly authenticated user and return that outcome
        register(user);
        return AUTHENTICATED;

    }


    /**
     * <p>Request creation of a new user profile.</p>
     */
    public String create() {

        // At this point, there is no special behavior needed
        // prior to executing the subdialog.  If there is, it
        // should likely be defined as an action in EditProfileActions
        // instead of here.
        return CREATE;

    }


    /**
     * <p>Alternate exit action for this dialog.  Remove the currently
     * logged on user (if any), remove this instance from session scope,
     * and return outcome <code>unauthenticated</code>.</p>
     */
    public String logoff() {

        unregister();
        return UNAUTHENTICATED;

    }


    /**
     * <p>Authenticate the entered username and password.</p>
     */
    public String logon() {

        // Attempt a successful authentication
        LogonLogic logic = (LogonLogic) getBean(LOGIC_BEAN);
        User user = logic.authenticate(username, password);
        if (user != null) {
            if (user.isConfirmed()) {
                // Confirmed user, log him/her on
                register(user);
                if (isRememberMe()) {
                    if (isRemember()) {
                        remember(user);
                    } else {
                        forget(user);
                    }
                }
                return AUTHENTICATED;
            } else {
                // Unconfirmed user, tell him/her to reply to the email
                error(messages.getMessage("profile.unconfirmed"));
                return UNAUTHENTICATED;
            }
        }

        // On unsuccessful authentication, tell the user to try again
        error(messages.getMessage("profile.incorrect"));
        return null;

    }


    // --------------------------------------------------------- Private Methods


    /**
     * <p>Remove any existing "remember me" cookie that was included.</p>
     *
     * @param user {@link User} to be forgotten
     */
    private void forget(User user) {

        FacesContext context = getFacesContext();
        HttpServletRequest request =
          (HttpServletRequest) context.getExternalContext().getRequest();
        Cookie cookie =
          new Cookie(COOKIE_NAME, "");
        cookie.setDomain(request.getServerName());
        cookie.setMaxAge(0); // Delete immediately
        cookie.setPath(request.getContextPath());
        HttpServletResponse response =
          (HttpServletResponse) context.getExternalContext().getResponse();
        response.addCookie(cookie);

    }


    /**
     * <p>Store the specified {@link User} in session scope, and take whatever
     * other actions are necessary to mark the user as being logged on.</p>
     *
     * @param user {@link User} who is to be logged on
     */
    private void register(User user) {

        // Store the user instance in session scope
        FacesContext context = getFacesContext();
        context.getExternalContext().getSessionMap().
          put(getUserKey(), user);
    
    }


    /**
     * <p>Add a "remember me" cookie to the current response.</p>
     *
     * @param user {@link User} whose identity is to be persisted
     */
    private void remember(User user) {

        FacesContext context = getFacesContext();
        HttpServletRequest request =
          (HttpServletRequest) context.getExternalContext().getRequest();
        Cookie cookie =
          new Cookie(COOKIE_NAME, "" + user.getId()); // FIXME - more secure mechanism needed
        // cookie.setDomain(request.getServerName());
        cookie.setMaxAge(60 * 60 * 24 * 365); // One year
        cookie.setPath(request.getContextPath());
        HttpServletResponse response =
          (HttpServletResponse) context.getExternalContext().getResponse();
        response.addCookie(cookie);

    }


    /**
     * <p>Remove registration of the currently logged in user, as needed.</p>
     */
    private void unregister() {

        // Remove user instance from session scope
        getFacesContext().getExternalContext().getSessionMap().
          remove(getUserKey());

    }


}
