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

package org.apache.shale.usecases.model.minimal;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import org.apache.shale.usecases.model.User;
import org.apache.shale.usecases.model.UsersDAO;

/**
 * <p>Minimal implementation of {@link UsersDAO} that provides no persistence
 * at all.  A single user (username="user", password="pass") is created at
 * initialization time.</p>
 *
 * $Id: MinimalUsersDAO.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class MinimalUsersDAO implements UsersDAO {

    
    // ------------------------------------------------------------ Constructors


    /**
     * <p>Construct an instance with a single valid user.</p>
     */
    public MinimalUsersDAO() {

        User user = new MinimalUser();
        user.setCategories(new int[0]);
        user.setConfirmed(true);
        user.setCreated(new Date());
        user.setEmailAddress("user@user.com");
        user.setFullName("Initial User");
        user.setId(1);
        user.setPassword("pass");
        user.setUpdated(user.getCreated());
        user.setUsername("user");
        users.add(user);

    }


    // ------------------------------------------------------ Instance Variables


    /**
     * <p><code>List</code> of valid {@link User} instances.</p>
     */
    private List users = new ArrayList();


    // -------------------------------------------------------- UsersDAO Methods


    // Specified by UsersDAO
    public User createUser() {

        User user = new MinimalUser();
        return user;

    }


    // Specified by UsersDAO
    public User findUser(int id) {

        synchronized (users) {
            for (int i = 0; i < users.size(); i++) {
                User user = (User) users.get(i);
                if (id == user.getId()) {
                    return user;
                }
            }
            return null;
        }

    }


    // Specified by UsersDAO
    public User findUser(String username) {

        synchronized (users) {
            for (int i = 0; i < users.size(); i++) {
                User user = (User) users.get(i);
                if (username.equals(user.getUsername())) {
                    return user;
                }
            }
            return null;
        }

    }


    // Specified by UsersDAO
    public void insertUser(User user) {

        synchronized (users) {
            user.setCreated(new Date());
            user.setId(calculateId());
            user.setUpdated(user.getCreated());
            User insert = new MinimalUser();
            insert.setCategories(user.getCategories());
            insert.setConfirmed(user.isConfirmed());
            insert.setCreated(user.getCreated());
            insert.setEmailAddress(user.getEmailAddress());
            insert.setFullName(user.getFullName());
            insert.setId(user.getId());
            insert.setPassword(user.getPassword());
            insert.setUpdated(user.getUpdated());
            insert.setUsername(user.getUsername());
            users.add(insert);
        }

    }


    // Specified by UsersDAO
    public void updateUser(User user) {

        synchronized (users) {
            User update = findUser(user.getId());
            if (update == null) {
                throw new IllegalArgumentException("" + user.getId());
            }
            user.setUpdated(new Date());
            update.setCategories(user.getCategories());
            update.setEmailAddress(user.getEmailAddress());
            update.setFullName(user.getFullName());
            update.setPassword(user.getPassword());
            update.setUpdated(user.getUpdated());
            update.setUsername(user.getUsername());
        }
        

    }


    // --------------------------------------------------------- Private Methods


    /**
     * <p>Calculate and return the next available user identifier.
     * <strong>WARNING</strong> - It is assumed that the caller has
     * already locked the <code>users</code> instance variable, to
     * avoid race conditions.</p>
     */
    private int calculateId() {

        int id = 0;
        Iterator items = users.iterator();
        while (items.hasNext()) {
            User item = (User) items.next();
            if (item.getId() > id) {
                id = item.getId();
            }
        }
        return id + 1;

    }


}
