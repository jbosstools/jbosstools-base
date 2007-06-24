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

import java.util.Date;
import org.apache.shale.usecases.model.User;

/**
 * <p>Minimal implementation of {@link User} with no persistence support.</p>
 *
 * $Id: MinimalUser.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class MinimalUser implements User {
    
    
    // -------------------------------------------------------------- Properties


    private int categories[] = new int[0];
    public int[] getCategories() { return this.categories; }
    public void setCategories(int categories[]) { this.categories = categories; }

    private boolean confirmed = false;
    public boolean isConfirmed() { return this.confirmed; }
    public void setConfirmed(boolean confirmed) { this.confirmed = confirmed; }

    private Date created = null;
    public Date getCreated() { return this.created; }
    public void setCreated(Date created) { this.created = created; }

    private String emailAddress = null;
    public String getEmailAddress() { return this.emailAddress; }
    public void setEmailAddress(String emailAddress) { this.emailAddress = emailAddress; }

    private String fullName = null;
    public String getFullName() { return this.fullName; }
    public void setFullName(String fullName) { this.fullName = fullName; }

    private int id = 0;
    public int getId() { return this.id; }
    public void setId(int id) { this.id = id; }

    private String password = null;
    public String getPassword() { return this.password; }
    public void setPassword(String password) { this.password = password; }

    private Date updated = null;
    public Date getUpdated() { return this.updated; }
    public void setUpdated(Date updated) { this.updated = updated; }

    private String username = null;
    public String getUsername() { return this.username; }
    public void setUsername(String username) { this.username = username; }


}
