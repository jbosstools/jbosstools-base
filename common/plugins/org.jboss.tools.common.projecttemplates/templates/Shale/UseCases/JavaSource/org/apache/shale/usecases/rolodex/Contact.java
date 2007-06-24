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

package org.apache.shale.usecases.rolodex;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

/**
 * <p>
 * Represents an entity in the rolodex.
 * </p>
 */
public class Contact {
    String name = null;

    String email = null;

    String residentialPhone = null;

    Address residentialAddress = null;

    String businessPhone = null;

    Address businessAddress = null;

    /**
     * <p>
     * Returns the first char of the name use by the rolodex index.
     * </p>
     */
    public char getTabIndex() {
        return getSortName().charAt(0);
    }

    /**
     * <p>
     * Returns the business {@link Address}.
     * </p>
     */
    public Address getBusinessAddress() {
        if (businessAddress == null)
            businessAddress = new Address();

        return businessAddress;
    }

    /**
     * <p>
     * Sets the business {@link Address}.
     * </p>
     */
    public void setBusinessAddress(Address businessAddress) {
        this.businessAddress = businessAddress;
    }

    /**
     * <p>
     * Returns the business phone number.
     * </p>
     */
    public String getBusinessPhone() {
        return businessPhone;
    }

    /**
     * <p>
     * Sets the business phone number.
     * </p>
     */
    public void setBusinessPhone(String businessPhone) {
        this.businessPhone = businessPhone;
    }

    /**
     * <p>
     * Returns the email address.
     * </p>
     */
    public String getEmail() {
        return email;
    }

    /**
     * <p>
     * Sets the email address.
     * </p>
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * <p>
     * Gets the entity name.
     * </p>
     */
    public String getName() {
        return name;
    }

    /**
     * <p>
     * Sets the entity name.
     * </p>
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * <p>
     * Gets the residential {@link Address}.
     * </p>
     */
    public Address getResidentialAddress() {
        if (residentialAddress == null) {
            residentialAddress = new Address();
        }
        return residentialAddress;
    }

    /**
     * <p>
     * Sets the residential {@link Address}.
     * </p>
     */
    public void setResidentialAddress(Address residentialAddress) {
        this.residentialAddress = residentialAddress;
    }

    /**
     * <p>
     * Gets the residential phone number.
     * </p>
     */
    public String getResidentialPhone() {
        return residentialPhone;
    }

    /**
     * <p>
     * Sets the residential phone number.
     * </p>
     */
    public void setResidentialPhone(String residentialPhone) {
        this.residentialPhone = residentialPhone;
    }

    /**
     * <p>Returns the <code>name</code> in upper case.</p>
     */
    public String getSortName() {
       if (name != null)
          return name.toUpperCase();
       
       return null;
    }
    
    /**
     * <p>Returns the <code>name</code> encoded.</p>
     */
    public String getEncodedName() {
        String encName = null;

        if (name != null) {
          try {
            encName =  URLEncoder.encode(name, "UTF-8");
          } catch (UnsupportedEncodingException e) {}          
       }
       return encName;
    }
    
    
}
