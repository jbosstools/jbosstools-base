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

package org.apache.shale.usecases.logic;

import org.apache.shale.usecases.model.User;
import org.apache.shale.usecases.model.UsersDAO;

/**
 * <p>Business logic for manipulating user profile information and
 * validating logon credentials.  The {@link UsersDAO} object used to
 * access the underlying persistent data must be injected before
 * any of the logic methods in this class may be used.</p>
 *
 * $Id: LogonLogic.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class LogonLogic {
    

    // -------------------------------------------------------------- Properties


    /**
     * <p>The {@link UsersDAO} object used to access our underlying
     * persistent data.</p>
     */
    private UsersDAO dao = null;
    public UsersDAO getDao() { return this.dao; }
    public void setDao(UsersDAO dao) { this.dao = dao; }
    

    // ---------------------------------------------------------- Public Methods


    /**
     * <p>Validate the specified logon credientials.  If valid credentials
     * are specified, return the corresponding {@link User} instance.
     * Otherwise, return <code>null</code>.</p>
     *
     * @param username Username creditial that was entered
     * @param password Password credential that was entered
     */
    public User authenticate(String username, String password) {

        if ((username == null) || (password == null)) {
            return null;
        }
        User user = dao.findUser(username);
        if (user == null) {
            return null;
        }
        if (password.equals(user.getPassword())) {
            return user;
        } else {
            return null;
        }

    }


    /**
     * <p>Create and return a {@link User} object that may be populated
     * and then passed to <code>insertUser()</code> for persistence.</p>
     */
    public User createUser() {

        return getDao().createUser();

    }


    /**
     * <p>Pass-through method to retrieve a {@link User} by id.</p>
     *
     * @param id Identifier of the {@link User} to return
     */
    public User findUser(int id) {

        return getDao().findUser(id);

    }


    /**
     * <p>Pass-through method to retrieve a {@link User} by username.</p>
     *
     * @param username Username of the {@link User} to return
     */
    public User findUser(String username) {

        return getDao().findUser(username);

    }


    /**
     * <p>Insert a newly created {@link User} into persistent storage.</p>
     *
     * @param user Created {@link User} to be persisted
     */
    public void insertUser(User user) {

        getDao().insertUser(user);

    }


    /**
     * <p>Update an existing {@link User} into persistent storage.</p>
     *
     * @param user Updated {@link User} to be persisted
     */
    public void updateUser(User user) {

        getDao().updateUser(user);

    }


}
