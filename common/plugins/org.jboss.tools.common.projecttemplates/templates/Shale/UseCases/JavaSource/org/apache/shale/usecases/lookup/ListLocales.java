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

package org.apache.shale.usecases.lookup;

import javax.faces.model.SelectItem;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.usecases.view.Domains;
import org.apache.shale.util.Messages;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>ViewController to retrieve the locales that are
 * supported by this application.</p>
 *
 * $Id: ListLocales.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class ListLocales extends AbstractViewController {
    
    
    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(ListLocales.class);


    /**
     * <p>Localized messages for this application.</p>
     */
    private static Messages messages =
      new Messages("org.apache.shale.usecases.view.Bundle");


    // -------------------------------------------------------------- Properties


    /**
     * <p>The set of supported locales for this application.</p>
     */
    private SelectItem supportedLocales[] = null;
    public SelectItem[] getSupportedLocales()
    { return this.supportedLocales; }
    public void setSupportedLocales(SelectItem[] supportedLocales)
    { this.supportedLocales = supportedLocales; }


    // -------------------------------------------------- Event Handling Methods


    // -------------------------------------------------- ViewController Methods


    /**
     * <p>If any calculations were required to acquire the data required to
     * perform this rendering, that logic would go in this method.</p>
     */
    public void prerender() {

        setSupportedLocales
          (((Domains) getBean("domains")).getSupportedLocales());

    }


    /**
     * <p>If any resources were allocated in the <code>prerender()</code> method
     * that were needed for rendering, and now need to be cleaned up, that
     * logic would go in this method.</p>
     */
    public void destroy() {

        setSupportedLocales(null);

    }


}
