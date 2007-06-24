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

/**
 * <p>Holds an addres for a {@link Contact}.</p>
 */
public class Address {
    String street1 = null;

    String street2 = null;

    String city = null;

    String state = null;

    int zip = 0;

    String province = null;

    String country = null;
    

    /**
     * <p>
     * Returns the City.
     * </p>
     */
    public String getCity() {
        return city;
    }

    /**
     * <p>
     * Sets the City.
     * </p>
     */
    public void setCity(String city) {
        this.city = city;
    }

    /**
     * <p>
     * Gets the Country.
     * </p>
     */
    public String getCountry() {
        return country;
    }

    /**
     * <p>
     * Sets the Country.
     * </p>
     */
    public void setCountry(String country) {
        this.country = country;
    }

    /**
     * <p>
     * Gets the Province.
     * </p>
     */
    public String getProvince() {
        return province;
    }

    /**
     * <p>
     * Sets the Province.
     * </p>
     */
    public void setProvince(String province) {
        this.province = province;
    }

    /**
     * <p>
     * Gets the State.
     * </p>
     */
    public String getState() {
        return state;
    }

    /**
     * <p>
     * Sets the State.
     * </p>
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * <p>
     * Gets the Street1.
     * </p>
     */
    public String getStreet1() {
        return street1;
    }

    /**
     * <p>
     * Sets the Street1.
     * </p>
     */
    public void setStreet1(String street1) {
        this.street1 = street1;
    }

    /**
     * <p>
     * Gets the Street2.
     * </p>
     */
    public String getStreet2() {
        return street2;
    }

    /**
     * <p>
     * Sets the Street2.
     * </p>
     */
    public void setStreet2(String street2) {
        this.street2 = street2;
    }

    /**
     * <p>
     * Gets the Zip Code.
     * </p>
     */
    public int getZip() {
        return zip;
    }

    /**
     * <p>
     * Sets the Zip Code.
     * </p>
     */
    public void setZip(int zip) {
        this.zip = zip;
    }

    
    /**
     * <p>
     * Gets the Zip Code.
     * </p>
     */
    public String getZipAsString() {
        return String.valueOf(zip);
    }

    /**
     * <p>
     * Sets the Zip Code.
     * </p>
     */
    public void setZipAsString(String zip) {
        try {
            this.zip = Integer.parseInt(zip);
        } catch (NumberFormatException e) {}
    }

}
