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

package org.apache.shale.usecases.locale;

import java.util.Iterator;
import java.util.Locale;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.util.Messages;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>ViewController to select the <code>Locale</code> to be used for
 * localizing responses.</p>
 *
 * $Id: Select.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Select extends AbstractViewController {
    
    
    // ------------------------------------------------------ Manifest Constants


    /**
     * <p>Logical outcome indicating that a locale was NOT successfully
     * selected.</p>
     */
    static final String FAILURE = "locale$failure";


    /**
     * <p>Logical outcome indicating that a locale was successfully selected.</p>
     */
    static final String SUCCESS = "locale$success";


    // -------------------------------------------------------- Static Variables


    /**
     * <p>The <code>Log</code> instance for this class.</p>
     */
    private static final Log log = LogFactory.getLog(Select.class);


    /**
     * <p>Localized messages for this application.</p>
     */
    private static Messages messages =
      new Messages("org.apache.shale.usecases.view.Bundle");


    // -------------------------------------------------------------- Properties


    /**
     * <p>The Stringified definition of the <code>Locale</code> to be used
     * for localizing responses.</p>
     */
    private String locale = null;
    public String getLocale() { return this.locale; }
    public void setLocale(String locale) { this.locale = locale; }


    // -------------------------------------------------- Event Handling Methods


    /**
     * <p>Set the new <code>Locale</code>, based on the value selected
     * by the user.</p>
     */
    public String select() {

        FacesContext context = FacesContext.getCurrentInstance();

        // Identify the locale String specified by the user
        String localeString = getLocale();
        if ((localeString == null) || (localeString.length() < 1)) {
            log.error(messages.getMessage("select.missing"));
            context.addMessage(null,
              new FacesMessage(messages.getMessage("select.missing")));
            return null;
        }

        // Match it to a Locale supported by this application
        Iterator locales = context.getApplication().getSupportedLocales();
        while (locales.hasNext()) {
            Locale locale = (Locale) locales.next();
            if (localeString.equals(locale.toString())) {
                if (log.isDebugEnabled()) {
                    log.debug(messages.getMessage("select.selected",
                                                  new Object[] { localeString }));
                }
                context.getViewRoot().setLocale(locale);
                return SUCCESS;
            }
        }

        // Error condition - no matching locale in the supported list
        String text = messages.getMessage("select.mismatch",
                                          new Object[] { localeString });
        log.warn(text);
        context.addMessage(null, new FacesMessage(text));
        return FAILURE;

    }


    // -------------------------------------------------- ViewController Methods


    /**
     * <p>Set the value of the <code>locale</code> property based on the
     * <code>Locale</code> in the current view.</p>
     */
    public void prerender() {

        Locale locale =
          FacesContext.getCurrentInstance().getViewRoot().getLocale();
        if (log.isTraceEnabled()) {
            log.trace(messages.getMessage("select.prerender",
                                          new Object[] { locale }));
        }
        setLocale(locale.toString());

    }


}
