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

package org.apache.shale.usecases.jndi;

import javax.faces.application.Application;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>Test page for JNDI access use case.</p>
 *
 * $Id: Test.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Test extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(Test.class);


    // -------------------------------------------------------------- Properties


    /**
     * <p>The calculated actual value.</p>
     */
    private String actual = null;


    /**
     * <p>Return the actual calculated value.</p>
     */
    public String getActual() {

        return actual;

    }


    /**
     * <p>Return the expected trace string for the first page, which is
     * only rendered.</p>
     */
    public String getExpected() {

        return "String Value/class java.lang.String/10/class java.lang.Integer/";

    }


    // -------------------------------------------------- ViewController Methods


    /**
     * <p>Calculate the actual value.</p>
     */
    public void prerender() {

        FacesContext context = FacesContext.getCurrentInstance();
        Application application = context.getApplication();
        ValueBinding vb = null;
        StringBuffer sb = new StringBuffer();

        vb = application.createValueBinding("#{jndi['env/String']}");
        sb.append("" + vb.getValue(context) + "/");
        sb.append("" + vb.getType(context) + "/");

        vb = application.createValueBinding("#{jndi['env/Integer']}");
        sb.append("" + vb.getValue(context) + "/");
        sb.append("" + vb.getType(context) + "/");

        actual = sb.toString();

    }


}
