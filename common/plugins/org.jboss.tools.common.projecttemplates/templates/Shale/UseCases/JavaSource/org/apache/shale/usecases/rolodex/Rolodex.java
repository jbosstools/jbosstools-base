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
import java.net.URLDecoder;
import java.util.List;

import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.shale.clay.component.Clay;
import org.apache.shale.clay.config.beans.AttributeBean;
import org.apache.shale.clay.config.beans.ComponentBean;
import org.apache.shale.clay.config.beans.ElementBean;
import org.apache.shale.view.AbstractViewController;

/**
 * <p>
 * The ViewController for the rolodex use case. This is a contacts list that is
 * to demonstrate the reuse ability for the Clay component.
 * </p>
 * 
 */
public class Rolodex extends AbstractViewController {

    /**
     * <p>
     * Commons logging utility object static instance.
     * </p>
     */
    private static Log log;
    static {
        log = LogFactory.getLog(Rolodex.class);
    }

    /**
     * <p>
     * The the selected tab index.
     * </p>
     */
    private int selectedTab = 0;

    /**
     * <p>
     * Returns the current tab index.
     * </p>
     */
    public int getSelectedTab() {
        return selectedTab;
    }

    /**
     * <p>
     * The contact that is selected for edit.
     * </p>
     */
    private Contact selectedContact = null;

    /**
     * <p>
     * Returns the selected contact for edit.
     * </p>
     */
    public Contact getSelectedContact() {
        return selectedContact;
    }

    /**
     * <p>
     * Sets the selected contact for edit.
     * </p>
     */
    public void setSelectedContact(Contact contact) {
        selectedContact = contact;
    }

    /**
     * <p>
     * Records a change in tab index.
     * </p>
     */
    public void setSelectedTab(int index) {
        if (log.isInfoEnabled())
            log.info("Switching from tab " + selectedTab + " to tab " + index);

        selectedTab = index;
    }

    /**
     * <p>
     * Creates an object graph uses to build the rolodex folder tabs dynamically.
     * </p>
     * 
     * @param item -
     *            SelectItem holding the information to create the tab link
     * @param context -
     *            FacesContext
     * @return returns a top level clay meta component bean
     */
    protected ElementBean createCommandLinkMetadata(SelectItem item,
            FacesContext context) {

        // create a command link attribute
        ElementBean link = new ElementBean();
        link.setRenderId(generateId());
        link.setJsfid("commandLink");
        link.setComponentType("javax.faces.HtmlCommandLink");

        // add a value attribute
        AttributeBean attr = new AttributeBean();
        attr.setName("value");
        attr.setValue(item.getLabel());
        link.addAttribute(attr);

        // turn on the the immediate attribute so the validation
        // logic is not invoked when switching tabs
        attr = new AttributeBean();
        attr.setName("immediate");
        attr.setValue("true");
        link.addAttribute(attr);

        // add a action method binding event when the link is clicked
        attr = new AttributeBean();
        attr.setName("action");
        attr.setValue("#{@managed-bean-name.changeTab}");
        link.addAttribute(attr);

        // create a parameter
        ElementBean param = new ElementBean();
        param.setJsfid("param");
        param.setComponentType("javax.faces.Parameter");

        // add a query param for the selected tab index
        attr = new AttributeBean();
        attr.setName("name");
        attr.setValue("tabIndex");
        param.addAttribute(attr);

        // add a query parameter for the tab index
        attr = new AttributeBean();
        attr.setName("value");
        attr.setValue(((Integer) item.getValue()).toString());
        param.addAttribute(attr);

        // add a parameter to the commandLink
        link.addChild(param);

        return link;
    }

    /**
     * <p>
     * A sequential counter to generate a unique renderId.
     * </p>
     */
    int renderId = 0;

    /**
     * <p>
     * Returns the next sequential renderId. Because the logic builds the list
     * in a linear fashion, a sequential unintelligent counter works.
     * </p>
     */
    private int generateId() {
        return renderId++;
    }

    /**
     * <p>
     * This method builds a verbatim meta tag. It is passed a "rendered"
     * expression that will be evaluated to determine if the component is
     * visible.
     * </p>
     * 
     * @param html
     *            The HTML tag to write to the document
     * @param renderExp
     *            A value binding EL for the "rendered" attribute.
     * @param context
     *            faces context
     * @return A clay element bean used to construct a faces outputText
     *         component.
     */
    protected ElementBean createVerbatimMetadata(String html, String renderExp,
            FacesContext context) {
        ElementBean text = createVerbatimMetadata(html, context);

        // add a rendered attribute
        AttributeBean attr = new AttributeBean();
        attr.setName("rendered");
        attr.setValue(renderExp);
        attr.setBindingType(AttributeBean.BINDING_TYPE_VALUE);
        text.addAttribute(attr);

        return text;

    }

    /**
     * <p>
     * This mehtod builds a simple verbatim meta tag. The value will not be
     * escaped.
     * <p>
     * 
     * @param html
     *            HTML tag to write to the document
     * @param context -
     *            faces context
     * @return A clay element bean used to construct a faces outputText
     *         component.
     */
    protected ElementBean createVerbatimMetadata(String html,
            FacesContext context) {

        // create an output Text
        ElementBean text = new ElementBean();
        text.setRenderId(generateId());
        text.setJsfid("outputText");
        text.setComponentType("javax.faces.HtmlOutputText");

        // add a value attribute
        AttributeBean attr = new AttributeBean();
        attr.setName("value");
        attr.setValue(html);
        text.addAttribute(attr);

        // add a escape attribute
        attr = new AttributeBean();
        attr.setName("escape");
        attr.setValue(Boolean.FALSE.toString());
        text.addAttribute(attr);
        
        // add a isTransient attribute
        attr = new AttributeBean();
        attr.setName("isTransient");
        attr.setValue(Boolean.TRUE.toString());
        text.addAttribute(attr);

        return text;
    }

    /**
     * <p>
     * This action event is fired when clicking on a tab link. The
     * <code>selectedTab</code> is set with the value of the
     * <code>tabIndex</code> request parameter.
     * </p>
     */
    public String changeTab() {
        if (log.isInfoEnabled())
            log.info("changeTab()");

        FacesContext context = FacesContext.getCurrentInstance();
        
        QueryParam paramObj = (QueryParam) getBean("queryParam");
        String tabIndex = paramObj.getTabIndex();
        
        if (tabIndex != null) {
            setSelectedTab(Integer.parseInt(tabIndex));

            // clear out the selected contact
            setSelectedContact(null);
        }

        return "rolodex$test";
    }

    /**
     * <p>
     * This is a method binding event fired from the <strong>Clay</strong>
     * component before building the component tree. The method signature is a
     * "Validator" event signature and the binding attribute is
     * <code>shapeValidator</code>.
     * </p>
     * 
     * @param context
     *            facesContext
     * @param component
     * @param displayElementRoot
     */
    public void createTabs(javax.faces.context.FacesContext context,
            javax.faces.component.UIComponent component,
            java.lang.Object displayElementRoot) {

        if (log.isInfoEnabled())
            log.info("createTabs()");

        // cast to the clay component, the one triggering the event
        Clay clayComponent = (Clay) component;

        // find the dao cached in application scope
        RolodexDao dao = (RolodexDao) getBean("rolodexDao");

        // return a list of tabs
        List tabs = dao.getTabs();

        ComponentBean root = (ComponentBean) displayElementRoot;
        root.setComponentType("javax.faces.HtmlPanelGroup");
        root.addChild(createVerbatimMetadata("<ul id=\"menu\">", context));

        for (int i = 0; i < tabs.size(); i++) {
            SelectItem item = (SelectItem) tabs.get(i);

            root.addChild(createVerbatimMetadata("<li id=\"", context));

            root.addChild(createVerbatimMetadata("nav-sel",
                    "#{@managed-bean-name.selectedTab == " + i + "}", context));
            root.addChild(createVerbatimMetadata("nav",
                    "#{@managed-bean-name.selectedTab != " + i + "}", context));

            root.addChild(createVerbatimMetadata("\">", context));
            root.addChild(createCommandLinkMetadata(item, context));
            root.addChild(createVerbatimMetadata("</li>", context));
        }

        root.addChild(createVerbatimMetadata("</ul>", context));

    }

    /**
     * <p>
     * This is called by the data table component to return a list of
     * {@link Contact}s that belong within the selected index.
     * </p>
     */
    public List getContactsForTab() {
        if (log.isInfoEnabled())
            log.info("getContactsForTab()");

        // find the dao cached in application scope
        RolodexDao dao = (RolodexDao) getBean("rolodexDao");

        // gets a list of contacts matching the selected tab index
        List contacts = dao.findContactsForTab(getSelectedTab());

        return contacts;
    }

    /**
     * <p>
     * This is an action event fired from clicking on a contact in the data
     * grid. The latest contact is located using the data access object and is
     * set as the "select" contact.
     * </p>
     */
    public String selectContact() {
        if (log.isInfoEnabled())
            log.info("selectContact()");

        FacesContext context = FacesContext.getCurrentInstance();

        // look for the commandLink query parameter
        QueryParam paramObj = (QueryParam) getBean("queryParam");
        String name = paramObj.getSelectedName();
        
        if (name != null) {
            // find the dao cached in application scope
            RolodexDao dao = (RolodexDao) getBean("rolodexDao");

            //decode value 
            try {
                name = URLDecoder.decode(name, "UTF-8");
            } catch (UnsupportedEncodingException e) {}
            
            // finds the selected contact by name
            setSelectedContact(dao.findContact(name));
        }

        return "rolodex$test";
    }

    /**
     * <p>
     * Saves a {@link Contact} to the mock data store.
     * </p>
     */
    public String saveContact() {
        if (log.isInfoEnabled())
            log.info("saveContact()");

        if (getSelectedContact() != null) {
            RolodexDao dao = (RolodexDao) getBean("rolodexDao");
            // the saveContact method will return the page the 
            // new contact will appear
            setSelectedTab(dao.saveContact(getSelectedContact()));
        }

        return "rolodex$test";
    }

    /**
     * <p>
     * Removes a {@link Contact} from the mock data store.
     * </p>
     */
    public String deleteContact() {
        if (log.isInfoEnabled())
            log.info("deleteContact()");

        if (getSelectedContact() != null) {
            RolodexDao dao = (RolodexDao) getBean("rolodexDao");
            dao.deleteContact(getSelectedContact());
            setSelectedContact(null);
        }

        return "rolodex$test";
    }

    /**
     * <p>
     * Creates a new {@link Contact} instance to be entered and saved.
     * </p>
     */
    public String newContact() {
       if (log.isInfoEnabled())
           log.info("newContact()");
               
        setSelectedContact(new Contact());
        
        return "rolodex$test";
    }

}
