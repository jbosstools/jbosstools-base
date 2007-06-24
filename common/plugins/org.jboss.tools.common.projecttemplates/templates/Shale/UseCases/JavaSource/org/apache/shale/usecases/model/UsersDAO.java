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

/**
 * <p>Data Access Object (DAO) for accessing model data about registered
 * users and their profile information.</p>
 *
 * $Id: UsersDAO.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public interface UsersDAO {
    

    /**
     * <p>Create and return a {@link User} object that may be populated
     * and then passed to <code>insertUser()</code> for persistence.</p>
     */
    public User createUser();


    /**
     * <p>Return the {@link User} for the corresponding user id, if any.
     * Otherwise, return <code>null</code>.</p>
     *
     * @param id User id to look up
     */
    public User findUser(int id);


    /**
     * <p>Return the {@link User} for the corresponding username, if any.
     * Otherwise, return <code>null</code>.</p>
     *
     * @param username Username to look up
     */
    public User findUser(String username);


    /**
     * <p>Insert a newly created {@link User} into persistent storage.</p>
     *
     * @param user Created {@link User} to be persisted
     */
    public void insertUser(User user);


    /**
     * <p>Update an existing {@link User} into persistent storage.</p>
     *
     * @param user Updated {@link User} to be persisted
     */
    public void updateUser(User user);


}
