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

package org.apache.shale.usecases.model;

import java.util.Date;

/**
 * <p>Model interface representing a user of the Use Cases application system.</p>
 *
 * $Id: User.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public interface User {
    

    // -------------------------------------------------------------- Properties


    /**
     * <p>Return the category identifiers of the message categories selected
     * by this user.</p>
     */
    public int[] getCategories();


    /**
     * <p>Set the category identifiers of the message categories selected
     * by this user.</p>
     *
     * @param categories The new category identifiers
     */
    public void setCategories(int categories[]);


    /**
     * <p>Return the flag indicating that this user's email address has
     * been confirmed.</p>
     */
    public boolean isConfirmed();


    /**
     * <p>Set the flag indicating that this user's email address has
     * been confirmed.</p>
     *
     * @param confirmed The new confirmed flag
     */
    public void setConfirmed(boolean confirmed);


    /**
     * <p>Return the "initially created" timestamp for this user.</p>
     */
    public Date getCreated();


    /**
     * <p>Set the "initially created" timestamp for this user.</p>
     *
     * @param created The new initially created timestamp
     */
    public void setCreated(Date created);


    /**
     * <p>Return the email address of this user.</p>
     */
    public String getEmailAddress();


    /**
     * <p>Set the email address of this user.</p>
     *
     * @param emailAddress The new email address
     */
    public void setEmailAddress(String emailAddress);


    /**
     * <p>Return the full name of this user.</p>
     */
    public String getFullName();


    /**
     * <p>Set the full name of this user.</p>
     *
     * @param fullName The new full name
     */
    public void setFullName(String fullName);


    /**
     * <p>Return the primary key for this user.</p>
     */
    public int getId();


    /**
     * <p>Set the primary key for this user.</p>
     *
     * @param id The new primary key
     */
    public void setId(int id);



    /**
     * <p>Return the password of this user.</p>
     */
    public String getPassword();


    /**
     * <p>Set the password of this user.</p>
     *
     * @param password The new password
     */
    public void setPassword(String password);


    /**
     * <p>Return the "last updated" timestamp for this user.</p>
     */
    public Date getUpdated();


    /**
     * <p>Set the "last updated" timestamp for this user.</p>
     *
     * @param updated The new last updated timestamp
     */
    public void setUpdated(Date updated);


    /**
     * <p>Return the username of this user.</p>
     */
    public String getUsername();


    /**
     * <p>Set the username of this user.</p>
     *
     * @param username The new username
     */
    public void setUsername(String username);


}
