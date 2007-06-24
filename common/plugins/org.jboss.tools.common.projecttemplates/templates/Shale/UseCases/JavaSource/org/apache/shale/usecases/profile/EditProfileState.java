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

import java.io.Serializable;

/**
 * <p>Dialog state information for the Edit Profile dialog.</p>
 *
 * $Id: EditProfileState.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class EditProfileState implements Serializable {


    // ------------------------------------------------ Configuration Properties


    /**
     * <p>Flag indicating that we are creating a new profile, versus editing
     * an existing one.</p>
     */
    private boolean creating = true;
    public boolean isCreating() { return this.creating; }
    public void setCreating(boolean creating) { this.creating = creating; }


    // -------------------------------------------------- Input Field Properties


    /**
     * <p>Category identifiers for the categories selected by this user
     */
    private int categories[] = new int[0];
    public int[] getCategories() { return this.categories; }
    public void setCategories(int categories[]) { this.categories = categories; }


    /**
     * <p>Email address for this user.</p>
     */
    private String emailAddress = null;
    public String getEmailAddress() { return this.emailAddress; }
    public void setEmailAddress(String emailAddress) { this.emailAddress = emailAddress; }


    /**
     * <p>Full name for this user.</p>
     */
    private String fullName = null;
    public String getFullName() { return this.fullName; }
    public void setFullName(String fullName) { this.fullName = fullName; }


    /**
     * <p>Password entered by the user.</p>
     */
    private String password = null;
    public String getPassword() { return this.password; }
    public void setPassword(String password) { this.password = password; }


    /**
     * <p>Confirmation password entered by the user.</p>
     */
    private String password2 = null;
    public String getPassword2() { return this.password2; }
    public void setPassword2(String password2) { this.password2 = password2; }


    /**
     * <p>Username entered by the user.</p>
     */
    private String username = null;
    public String getUsername() { return this.username; }
    public void setUsername(String username) { this.username = username; }


}
