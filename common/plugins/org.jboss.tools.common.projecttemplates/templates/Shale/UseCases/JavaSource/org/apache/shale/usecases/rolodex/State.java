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
 * <p>Represents a State domain object.</p>
 */
public class State {
    private String abbrState = null;

    private String state = null;

    private String country = null;
    
    private String abbrCountry = null;

    /**
     * @return Returns the abbrCountry.
     */
    public String getAbbrCountry() {
        return abbrCountry;
    }
    

    /**
     * @param abbrCountry The abbrCountry to set.
     */
    public void setAbbrCountry(String abbrCountry) {
        this.abbrCountry = abbrCountry;
    }
    

    /**
     * @return Returns the abbrState.
     */
    public String getAbbrState() {
        return abbrState;
    }

    /**
     * @param abbrState
     *            The abbrState to set.
     */
    public void setAbbrState(String abbrState) {
        this.abbrState = abbrState;
    }

    /**
     * @return Returns the country.
     */
    public String getCountry() {
        return country;
    }

    /**
     * @param country
     *            The country to set.
     */
    public void setCountry(String country) {
        this.country = country;
    }

    /**
     * @return Returns the state.
     */
    public String getState() {
        return state;
    }

    /**
     * @param state
     *            The state to set.
     */
    public void setState(String state) {
        this.state = state;
    }

}
