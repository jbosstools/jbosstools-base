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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.usecases.logic.LogonLogic;
import org.apache.shale.usecases.model.User;
import org.apache.shale.util.Messages;
import org.apache.shale.view.AbstractFacesBean;

/**
 * <p>Action methods for the Edit Profile dialog.</p>
 *
 * $Id: EditProfileActions.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class EditProfileActions extends AbstractFacesBean {
    

    // -------------------------------------------------------- Static Variables


    /**
     * <p>Log instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(EditProfileActions.class);


    /**
     * <p>Message resources for this application.</p>
     */
    private static final Messages messages =
      new Messages("org.apache.shale.usecases.view.Bundle");


    // ------------------------------------------------------ Manifest Constants


    /**
     * <p>Managed bean name under which the business logic bean instance
     * for this dialog is stored.</p>
     */
    static final String LOGIC_BEAN = "profile$logic"; // FIXME - shared name


    /**
     * <p>Logical outcome indicating a problem with the password field.</p>
     */
    static final String PASSWORD = "password";


    /**
     * <p>Logical outcome indicating successful completion.</p>
     */
    static final String SUCCESS = "success";


    /**
     * <p>Logical outcome indicating a problem with the username field.</p>
     */
    static final String USERNAME = "username";



    // --------------------------------------------------- Configured Properties


    /**
     * <p>Flag indicating that a confirmation email is required before a new
     * profile may be activated.</p>
     */
    private boolean confirmation = false;
    public boolean isConfirmation() { return this.confirmation; }
    public void setConfirmation(boolean confirmation) { this.confirmation = confirmation; }


    /**
     * <p>Session scope attribute under which a {@link User} instance for the
     * currently logged in user is stored.</p>
     */
    private String userKey = "user";
    public String getUserKey() { return this.userKey; }
    public void setUserKey(String userKey) { this.userKey = userKey; }


    // ----------------------------------------------------------------- Actions


    /**
     * <p>Perform any processing necessary to cancel an edit profile dialog.</p>
     *
     * <p>The following logical outcome values are returned:</p>
     * <ul>
     * <li><code>SUCCESS</code> - Proceed to the next state normally.</li>
     * </ul>
     */
    public String cancel() {

        return SUCCESS;

    }


    /**
     * <p>Finish processing by creating or updating the appropriate user
     * profile.</p>
     *
     * <p>The following logical outcome values are returned:</p>
     * <ul>
     * <li><code>PASSWORD</code> - One or both password values were entered,
     *     but they do not match (or neither were entered on a create)</li>
     * <li><code>SUCCESS</code> - Creation or update succeeded, proceed to
     *     the next state normally.</li>
     * <li><code>USERNAME</code> - Missing or dupicate username on
     *     a create.</li>
     * </ul>
     */
    public String finish() {

        // Acquire our state information
        EditProfileState state = (EditProfileState) getValue("#{dialog.data}");
        boolean okUsername = true;
        boolean okPassword = true;

        // Validate password match if both specified
        if ((state.getPassword() != null) && (state.getPassword().length() > 0) &&
            (state.getPassword2() != null) && (state.getPassword2().length() > 0)) {
            if (!state.getPassword().equals(state.getPassword2())) {
                error(messages.getMessage("profile.mismatch"));
                okPassword = false;
            }
        }

        // Validate required fields if creating
        if (state.isCreating()) {
            if ((state.getUsername() == null) || (state.getUsername().length() < 1)) {
                error(messages.getMessage("profile.username"));
                okUsername = false;
            }
            if ((state.getPassword() == null) || (state.getPassword().length() < 1)) {
                error(messages.getMessage("profile.password"));
                okPassword = false;
            }
        }

        // Validate duplicate username if creating
        LogonLogic logic = (LogonLogic) getBean(LOGIC_BEAN);
        if (state.isCreating() && (logic.findUser(state.getUsername()) != null)) {
            error(messages.getMessage("profile.duplicate"));
            okUsername = false;
        }

        // Return appropriate outcome on validation failures
        if (!okUsername) {
            return USERNAME;
        } else if (!okPassword) {
            return PASSWORD;
        }

        // Create or acquire our User instance
        User user = null;
        if (state.isCreating()) {
            user = logic.createUser();
        } else {
            user = (User) getBean(getUserKey());
        }

        // Update to reflect changes during this dialog
        user.setCategories(state.getCategories());
        if (state.isCreating()) {
            user.setConfirmed(!isConfirmation());
        }
        user.setEmailAddress(state.getEmailAddress());
        user.setFullName(state.getFullName());
        if ((state.getPassword() != null) && (state.getPassword().length() > 0)) {
            user.setPassword(state.getPassword());
        }
        user.setUsername(state.getUsername());

        // Persist the changes made during this dialog
        if (state.isCreating()) {
            logic.insertUser(user);
        } else {
            logic.updateUser(user);
        }

        // Log in a new user if already confirmed
        // Otherwise, send the confirmation email
        if (state.isCreating()) {
            if (user.isConfirmed()) {
                getSessionMap().put(getUserKey(),user);
            } else {
                info(messages.getMessage("profile.confirm"));
                // FIXME - send confirmation email
            }
        }

        // Return a success outcome
        return SUCCESS;


    }


    /**
     * <p>Register an instance of {@link EditProfileState} as the data object
     * for our dialog.  If there is an existing logged on user, we are editing
     * the existing profile; otherwise, we are creating a new one.</p>
     *
     * <p>The following logical outcome values are returned:</p>
     * <ul>
     * <li><code>SUCCESS</code> - Proceed to the next state.</li>
     * </ul>
     */
    public String setup() {

        // Acquire a reference to the currently logged on user, if there is one
        User user = (User) getBean(getUserKey());

        // Configure a state object based on the currently logged on user,
        // or set up for a newly created profile
        EditProfileState state = new EditProfileState();
        if (user == null) {
            state.setCreating(true);
        } else {
            state.setCreating(false);
            int oldCategories[] = user.getCategories();
            int categories[] = null;
            if (oldCategories != null) {
                categories = new int[oldCategories.length];
                System.arraycopy
                  (oldCategories, 0, categories, 0, oldCategories.length);
            } else {
                categories = new int[0];
            }
            state.setCategories(categories);
            state.setEmailAddress(user.getEmailAddress());
            state.setFullName(user.getFullName());
            state.setPassword(null);
            state.setPassword2(null);
            state.setUsername(user.getUsername());
        }

        // Register our state as the data object for this dialog, and continue
        setValue("#{dialog.data}", state);
        return SUCCESS;

    }


}
