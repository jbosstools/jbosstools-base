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

package org.apache.shale.usecases.ajax;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>View controller for Ajax code completion test case.</p>
 *
 * $Id: Completion.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Completion extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(Completion.class);


    // -------------------------------------------------------------- Properties


    /**
     * <p>The state name to be entered.</p>
     */
    private String name = null;


    /**
     * <p>Return the state name to be entered.</p>
     */
    public String getName() {

        return this.name;

    }


    /**
     * <p>Set the state name to be entered.</p>
     *
     * @param name The new state name
     */
    public void setName(String name) {

        this.name = name;

    }


    /**
     * <p>The result stored by the submit action.</p>
     */
    private String result = null;


    /**
     * <p>Return the result stored by the submit action.</p>
     */
    public String getResult() {

        return this.result;

    }


    /**
     * <p>Set the result stored by the submit action.</p>
     *
     * @param result The new result
     */
    public void setResult(String result) {

        this.result = result;

    }


    // ------------------------------------------------- Component Event Methods


    /**
     * <p>Store submitted value in the result property.</p>
     */
    public String submit() {

        setResult("User submitted: " + getName());
        setName(null);
        return null; // Redisplay the current page

    }


    // -------------------------------------------------- ViewController Methods


}
