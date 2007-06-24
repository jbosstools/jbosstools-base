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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.faces.model.SelectItem;

import org.apache.commons.digester.Digester;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * <p>
 * Data Access Object for the roledex use case.
 * </p>
 */
public class RolodexDao {

    /**
     * <p>
     * Commons logging utility object static instance.
     * </p>
     */
    private static Log log;
    static {
        log = LogFactory.getLog(RolodexDao.class);
    }

    /**
     * <p>
     * Mock datastore.
     * </p>
     */
    private Collection entityDataStore = null;

    private Collection stateDataStore = null;

    private Collection countryDataStore = null;

    /**
     * <p>
     * The constructor loads some default data.
     * </p>
     */
    public RolodexDao() {
        GenericComparator comparator = new GenericComparator();
        comparator.setSortBy("sortName, name");

        entityDataStore = new TreeSet(comparator);

        comparator = new GenericComparator();
        comparator.setSortBy("value");

        stateDataStore = new TreeSet(comparator);
        countryDataStore = new TreeSet(comparator);
        loadStates();
    }

    /**
     * <p>
     * Tab indexes for the rolodex. Each sub array represents the starting and
     * ending character index.
     * </p>
     */
    public final static char[][] TAB_INDEX = { { 'A', 'B' }, { 'C', 'D' },
            { 'E', 'F' }, { 'G', 'H' }, { 'I', 'J' }, { 'K', 'L' },
            { 'M', 'N' }, { 'O', 'P' }, { 'Q', 'Z' } };

    /**
     * <p>
     * Returns a list of <code>SelectItem</code> that will be used to build
     * the links.
     * </p>
     */
    public List getTabs() {
        List tabs = new ArrayList();
        SelectItem tab = null;

        for (int i = 0; i < TAB_INDEX.length; i++) {
            tab = new SelectItem();
            tab.setLabel(TAB_INDEX[i][0] + "-" + TAB_INDEX[i][1]);
            tab.setValue(new Integer(i));
            tabs.add(tab);
        }

        return tabs;
    }

    /**
     * <p>
     * Saves a {@link Contact} to the mock data store.
     * </p>
     */
    public int saveContact(Contact entity) {
        entityDataStore.add(entity);
        return findTabForContact(entity);
    }

    /**
     * <p>This function will find the tabIndex that the {@link Contact}
     * will be located on.  It will default to the first page.</p>
     */
    public int findTabForContact(Contact contact) {
        for (int i = 0; i < TAB_INDEX.length; i++) {
           if ((contact.getTabIndex() >= TAB_INDEX[i][0]) && 
               (contact.getTabIndex() <= TAB_INDEX[i][1]))  
              return i;    
        }
        
        return 0;
    }
    
    
    /**
     * <p>
     * Retuns a subset of contacts within the <code>index</code> of a a tab
     * defined by <code>TAB_INDEX</code> from the mock data store. If this was
     * a RDBMS data store, the contacts returned might be a "ghost" object
     * meaning that maybe only the <code>name</code> attribute would be
     * populated.
     * </p>
     */
    public List findContactsForTab(int index) {
        List contacts = new ArrayList();

        Contact low = new Contact();
        Contact high = new Contact();

        StringBuffer key = new StringBuffer();
        
        key.append(TAB_INDEX[index][0]);
        low.setName(key.toString());

        key.setLength(0);
        key.append(TAB_INDEX[index][1]);
        for (int i = 0; i < 50; i++)
           key.append('z');

        high.setName(key.toString());

        SortedSet subSet = ((SortedSet) entityDataStore).subSet(low, high);
        Iterator si = subSet.iterator();
        while (si.hasNext())
            contacts.add(si.next());

        return contacts;
    }

    /**
     * <p>
     * Removes the contact from the mock data store.
     * </p>
     */
    public void deleteContact(Contact entity) {
        entityDataStore.remove(entity);
    }

    /**
     * <p>
     * Loads the {@link State} codes and contacts from an XML data source. The
     * <code>stateDataStore</code> set will hold the states where the
     * <code>countryDataStore</code> set will hold the countries. The target type of
     * these collections will be SelectItem.  The contacts are held in the 
     * <code>entityDataStore</code> set.
     * </p>
     */
    public void loadStates() {

        Object config = new Object() {
            public void addState(State state) {
                SelectItem item = null;
                // add a blank option
                if (stateDataStore.size() == 0) {
                    item = new SelectItem();
                    item.setLabel("");
                    item.setValue("");
                    stateDataStore.add(item);
                }
                
                item = new SelectItem();
                item.setLabel(state.getState());
                item.setValue(state.getAbbrState());
                stateDataStore.add(item);

                item = new SelectItem();
                item.setLabel(state.getCountry());
                item.setValue(state.getAbbrCountry());
                countryDataStore.add(item);
            }

            public void addContact(Contact contact) {
                saveContact(contact);
            }
        };

        Digester digester = new Digester();
        digester.setValidating(false);
        digester.setUseContextClassLoader(true);

        digester.addObjectCreate("dex/states/state",
                org.apache.shale.usecases.rolodex.State.class);
        digester.addSetProperties("dex/states/state");
        digester.addSetNext("dex/states/state", "addState",
                "org.apache.shale.usecases.rolodex.State");

        digester.addObjectCreate("dex/contacts/contact/residentialAddress",
                org.apache.shale.usecases.rolodex.Address.class);
        digester.addCallMethod(
                "dex/contacts/contact/residentialAddress/street1",
                "setStreet1", 0);
        digester.addCallMethod(
                "dex/contacts/contact/residentialAddress/street2",
                "setStreet2", 0);
        digester.addCallMethod("dex/contacts/contact/residentialAddress/city",
                "setCity", 0);
        digester.addCallMethod("dex/contacts/contact/residentialAddress/state",
                "setState", 0);
        digester.addCallMethod("dex/contacts/contact/residentialAddress/zip",
                "setZipAsString", 0);
        digester.addCallMethod(
                "dex/contacts/contact/residentialAddress/province",
                "setProvince", 0);
        digester.addCallMethod(
                "dex/contacts/contact/residentialAddress/country",
                "setCountry", 0);
        digester.addSetNext("dex/contacts/contact/residentialAddress",
                "setResidentialAddress",
                "org.apache.shale.usecases.rolodex.Address");

        digester.addObjectCreate("dex/contacts/contact/businessAddress",
                org.apache.shale.usecases.rolodex.Address.class);
        digester.addCallMethod("dex/contacts/contact/businessAddress/street1",
                "setStreet1", 0);
        digester.addCallMethod("dex/contacts/contact/businessAddress/street2",
                "setStreet2", 0);
        digester.addCallMethod("dex/contacts/contact/businessAddress/city",
                "setCity", 0);
        digester.addCallMethod("dex/contacts/contact/businessAddress/state",
                "setState", 0);
        digester.addCallMethod("dex/contacts/contact/businessAddress/zip",
                "setZipAsString", 0);
        digester.addCallMethod("dex/contacts/contact/businessAddress/province",
                "setProvince", 0);
        digester.addCallMethod("dex/contacts/contact/businessAddress/country",
                "setCountry", 0);
        digester.addSetNext("dex/contacts/contact/businessAddress",
                "setBusinessAddress",
                "org.apache.shale.usecases.rolodex.Address");

        digester.addObjectCreate("dex/contacts/contact",
                org.apache.shale.usecases.rolodex.Contact.class);
        digester.addCallMethod("dex/contacts/contact/name", "setName", 0);
        digester.addCallMethod("dex/contacts/contact/email", "setEmail", 0);
        digester.addCallMethod("dex/contacts/contact/residentialPhone",
                "setResidentialPhone", 0);
        digester.addCallMethod("dex/contacts/contact/businessPhone",
                "setBusinessPhone", 0);
        digester.addSetNext("dex/contacts/contact", "addContact",
                "org.apache.shale.usecases.rolodex.Contact");

        digester.push(config);

        ClassLoader classloader = Thread.currentThread()
                .getContextClassLoader();
        if (classloader == null)
            classloader = config.getClass().getClassLoader();

        InputSource in = new InputSource(
                classloader
                        .getResourceAsStream("org/apache/shale/usecases/rolodex/dex.xml"));

        try {
            digester.parse(in);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (SAXException e) {
            e.printStackTrace();
        }

    }

    /**
     * <p>
     * Returns an array of states used to populate a select list. The target
     * type is an array of SelectItem.
     * </p>
     */
    public SelectItem[] getStates() {
        SelectItem[] states = new SelectItem[stateDataStore.size()];
        stateDataStore.toArray(states);
        return states;
    }

    /**
     * <p>
     * Returns an array of countries used to populate a select list. The target
     * type is an array of SelectItem.
     * </p>
     */
    public SelectItem[] getCountries() {
        SelectItem[] countries = new SelectItem[countryDataStore.size()];
        countryDataStore.toArray(countries);
        return countries;
    }

    /**
     * <p>
     * Returns the latest copy of the {@link Contact} by primary key.
     * </p>
     * 
     * @param name
     *            contact name that uniquely identifies a Contact.
     * @return the target, fully populated {@link Contact}
     */
    public Contact findContact(String name) {
        Contact sarg = new Contact();
        sarg.setName(name);
        SortedSet subSet = ((TreeSet) entityDataStore).tailSet(sarg);
        return (Contact) subSet.first();
    }

}
