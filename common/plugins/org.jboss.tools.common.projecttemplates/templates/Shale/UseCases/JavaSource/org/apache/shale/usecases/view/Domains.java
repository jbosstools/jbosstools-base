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

package org.apache.shale.usecases.view;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.application.ApplicationFactory;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import org.apache.shale.util.Messages;

/**
 * <p>Utility class to return locale-specific domains (lists of selection items)
 * based on the current <code>Locale</code> of this request.  An instance of
 * this class will typically be defined as an application scope managed bean,
 * so that it is instantiated on demand.</p>
 *
 * $Id: Domains.java,v 1.1 2005/12/23 14:25:52 glory Exp $
 */
public class Domains {
    

    // -------------------------------------------------------- Static Variables


    /**
     * <p>Localized messages for this class.</p>
     */
    private static Messages messages =
      new Messages("org.apache.shale.usecases.view.Bundle");


    // ------------------------------------------------------ Instance Variables


    /**
     * <p><code>Map</code> containing arrays of <code>SelectItem</code>s
     * representing the supported message categories for this application,
     * keyed by the Locale in which the descriptions have been localized.</p>
     */
    private Map categories = new HashMap();


    /**
     * <p><code>Map</code> containing arrays of <code>SelectItem</code>s
     * representing the locales supported by this application, keyed by
     * the Locale in which the descriptions have been localized.</p>
     */
    private Map locales = new HashMap();


    /**
     * <p>Array of <code>SelectItem</code> representing the abbreviations
     * and descriptions of all valid US states.</p>
     */
    private SelectItem[] states = {
        new SelectItem("AL", "Alabama"),
        new SelectItem("AK", "Alaska"),
        new SelectItem("AZ", "Arizona"),
        new SelectItem("AR", "Arkansas"),
        new SelectItem("CA", "California"),
        new SelectItem("CO", "Colorado"),
        new SelectItem("CT", "Connecticut"),
        new SelectItem("DE", "Delaware"),
        new SelectItem("DC", "District of Columbia"),
        new SelectItem("FL", "Florida"),
        new SelectItem("GA", "Georgia"),
        new SelectItem("HI", "Hawaii"),
        new SelectItem("ID", "Idaho"),
        new SelectItem("IL", "Illinois"),
        new SelectItem("IN", "Indiana"),
        new SelectItem("IA", "Iowa"),
        new SelectItem("KS", "Kansas"),
        new SelectItem("KY", "Kentucky"),
        new SelectItem("LA", "Louisiana"),
        new SelectItem("ME", "Maine"),
        new SelectItem("MD", "Maryland"),
        new SelectItem("MA", "Massachusetts"),
        new SelectItem("MI", "Michigan"),
        new SelectItem("MN", "Minnesota"),
        new SelectItem("MS", "Mississippi"),
        new SelectItem("MO", "Missouri"),
        new SelectItem("MT", "Montana"),
        new SelectItem("NE", "Nebraska"),
        new SelectItem("NV", "Nevada"),
        new SelectItem("NH", "New Hampshire"),
        new SelectItem("NJ", "New Jersey"),
        new SelectItem("NM", "New Mexico"),
        new SelectItem("NY", "New York"),
        new SelectItem("NC", "North Carolina"),
        new SelectItem("ND", "North Dakota"),
        new SelectItem("OH", "Ohio"),
        new SelectItem("OK", "Oklahoma"),
        new SelectItem("OR", "Oregon"),
        new SelectItem("PA", "Pennyslvania"),
        new SelectItem("RI", "Rhode Island"),
        new SelectItem("SC", "South Carolina"),
        new SelectItem("SD", "South Dakota"),
        new SelectItem("TN", "Tennessee"),
        new SelectItem("TX", "Texas"),
        new SelectItem("UT", "Utah"),
        new SelectItem("VT", "Vermont"),
        new SelectItem("VA", "Virginia"),
        new SelectItem("WA", "Washington"),
        new SelectItem("WV", "West Virginia"),
        new SelectItem("WI", "Wisconsin"),
        new SelectItem("WY", "Wyoming")
    };


    /**
     * <p>Array of state names in alphabetical order (lazily instantiated).</p>
     */
    private String[] stateNames = null;


    // -------------------------------------------------------------- Properties


    /**
     * <p>Return an array of selection items representing valid US state
     * abbreviations and descriptions.</p>
     */
    public SelectItem[] getStates() {

        return this.states;

    }


    /**
     * <p>Return an array of state names in alphabetical order.</p>
     */
    public String[] getStateNames() {

        if (stateNames == null) {
            stateNames = new String[states.length];
            for (int i = 0; i < stateNames.length; i++) {
                stateNames[i] = states[i].getLabel();
            }
            Arrays.sort(stateNames);
	}
        return stateNames;

    }


    /**
     * <p>Return an array of selection items representing the message categories
     * supported by this application, with the labels localized based on the
     * <code>Locale</code> of the current request.</p>
     */
    public SelectItem[] getSupportedCategories() {

        // Return any previously cached array for this request locale
        Locale locale =
          FacesContext.getCurrentInstance().getViewRoot().getLocale();
        return getSupportedCategories(locale);

    }


    /**
     * <p>Return an array of selection items representing the message categories
     * supported by this application, with the labels localized based on the
     * specified <code>Locale</code>.</p>
     */
    public SelectItem[] getSupportedCategories(Locale locale) {

        // Return any previously cached array for the specfiied locale
        SelectItem items[] = null;
        synchronized (categories) {
            items = (SelectItem[]) categories.get(locale);
            if (items != null) {
                return items;
            }
        }

        // Construct and cache a new array, before returning it
        SelectItem item = null;
        List list = new ArrayList();
        int id = 0;
        String label = null;
        while (true) {
            label = messages.getMessage("category." + id, locale);
            if (label == null) {
                break;
            }
            list.add(new SelectItem(new Integer(id), label));
            id++;
        }
        items = (SelectItem[]) list.toArray(new SelectItem[list.size()]);
        synchronized(categories) {
            categories.put(locale, items);
        }
        return items;

    }



    /**
     * <p>Return an array of selection items representing the locales supported
     * by this application, with the labels localized based on the
     * <code>Locale</code> of the current request.</p>
     */
    public SelectItem[] getSupportedLocales() {

        Locale locale =
          FacesContext.getCurrentInstance().getViewRoot().getLocale();
        return getSupportedLocales(locale);

    }

    
    /**
     * <p>Return an array of selection items representing the locales supported
     * by this application, with the labels localized based on the specified
     * <code>Locale</code>.</p>
     *
     * @param locale <code>Locale</code> used to localize the labels
     */
    public SelectItem[] getSupportedLocales(Locale locale) {

        // Return any previously cached array for the specified locale
        SelectItem items[] = null;
        synchronized (locales) {
            items = (SelectItem[]) locales.get(locale);
            if (items != null) {
                return items;
            }
        }

        // Construct and cache a new array, before returning it
        SelectItem item = null;
        List list = new ArrayList();
        ApplicationFactory afactory = (ApplicationFactory)
          FactoryFinder.getFactory(FactoryFinder.APPLICATION_FACTORY);
        Application application = afactory.getApplication();
        Iterator supporteds = application.getSupportedLocales();
        while (supporteds.hasNext()) {
            Locale supported = (Locale) supporteds.next();
            item = new SelectItem(supported.toString(),
                                  messages.getMessage("locale." + supported.toString(),
                                                      locale));
            list.add(item);
        }
        items = (SelectItem[]) list.toArray(new SelectItem[list.size()]);
        synchronized(locales) {
            locales.put(locale, items);
        }
        return items;


    }

}
