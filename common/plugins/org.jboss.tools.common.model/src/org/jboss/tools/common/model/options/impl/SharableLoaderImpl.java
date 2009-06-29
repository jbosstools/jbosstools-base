/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.options.impl;

import java.util.Properties;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XChild;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.model.XModelConstants;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.options.SharableConstants;
import org.jboss.tools.common.model.options.SharableElement;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class SharableLoaderImpl implements SharableConstants {

    public SharableLoaderImpl() {}

    public void loadSharable(Element element, SharableElement sharable, String scopename) {
        sharable.setScope(scopename);
        loadAttributes(element, sharable, scopename);
        loadChildren(element, sharable, scopename);
        String precisescope = element.getAttribute(SCOPE);
        sharable.setScope(reduceScopeName(scopename, precisescope));
    }
    
	XModelObjectLoaderUtil loader = new XModelObjectLoaderUtil();

    public void loadAttributes(Element element, SharableElement sharable, String scopename) {
        XAttribute[] an = sharable.getModelEntity().getAttributes();
        for (int i = 0; i < an.length; i++) {
            String xml = an[i].getXMLName();
            if(xml == null || xml.trim().length() == 0) continue;
            String v = loader.getAttribute(element, xml);
            if(v != null && "Note2".equals(an[i].getEditor().getName())) { //$NON-NLS-1$
            	v = XModelObjectLoaderUtil.loadFromXMLAttribute(v);
            }
            if(v != null) sharable.setAttributeValue(an[i].getName(), v);
        }
    }

    public void loadChildren(Element element, SharableElement sharable, String scopename) {
        NodeList nl = element.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node c = nl.item(i);
            if(c.getNodeType() != Node.ELEMENT_NODE) continue;
            loadChild((Element)c, sharable, scopename);
        }
    }

    public void loadChild(Element element, SharableElement sharable, String scopename) {
        String en = XModelObjectLoaderUtil.getModelEntityAttribute(element);
        if(en == null) return;
        XModelEntity ent = sharable.getModel().getMetaData().getEntity(en);
        if(ent == null) return;
        boolean hasName = (ent.getAttribute(XModelObjectConstants.ATTR_NAME) != null);
        String name = (!hasName) ? element.getNodeName()
                      : element.getAttribute(XModelObjectConstants.XML_ATTR_NAME);
        SharableElement sc = sharable.findSharableChild(name);
        if(sc == null) {
            Properties p = new Properties();
            if(hasName) p.setProperty(XModelObjectConstants.ATTR_NAME, name);
            sc = (SharableElement)sharable.getModel().createModelObject(en, p);
            sc.setScopeExists(PROJECT, false);
            sharable.addChild(sc);
            if(!hasName) sharable.setName(name);
        }
        if(sc != null) {
            if(sc instanceof SharableContainerImpl)
              new SharableContainerLoader().loadSharable(element, sc, scopename);
            else
              loadSharable(element, sc, scopename);
        }
    }

    //// save

    public void saveSharable(Element parent, SharableElement sharable, String scopename) {
        if(!sharable.scopeExists(scopename)) return;
        XModelEntity entity = sharable.getModelEntity();
        String en = entity.getName();
        if(en.startsWith("Sharable")) en = en.substring(8); //$NON-NLS-1$
        Element element = parent.getOwnerDocument().createElement(en);
        parent.appendChild(element);
        saveAttributes(element, sharable, scopename);
        saveChildren(element, sharable, scopename);
    }

    public void saveAttributes(Element element, SharableElement sharable, String scopename) {
        XModelEntity entity = sharable.getModelEntity();
        element.setAttribute(XModelConstants.XMODEL_ENTITY_ATTR, entity.getName());
        XAttribute[] as = sharable.getModelEntity().getAttributes();
        element.setAttribute(SCOPE, reduceScopeName(scopename, sharable.getScope()));
        for (int i = 0; i < as.length; i++) {
            if(as[i].isFake()) continue;
            String xml = as[i].getXMLName();
            if(xml == null || xml.trim().length() == 0) continue;
            String v = sharable.getAttributeValue(as[i].getName(), scopename);
            if(v != null && "Note2".equals(as[i].getEditor().getName())) { //$NON-NLS-1$
            	v = XModelObjectLoaderUtil.saveToXMLAttribute(v);
            }            
            loader.saveAttribute(element, xml, v);
        }
    }

    public void saveChildren(Element element, SharableElement sharable, String scopename) {
        SharableElement[] se = sharable.getSharableChildren();
        for (int i = 0; i < se.length; i++) {
            if(se[i] != null) {
                if(se[i] instanceof SharableContainerImpl)
                  new SharableContainerLoader().saveSharable(element, se[i], scopename);
                else
                  saveSharable(element, se[i], scopename);
            }
        }
    }
    public String reduceScopeName(String currentscope, String precisescope) {
/*
        if(precisescope == null || precisescope.length() == 0) return currentscope;
        return (GENERAL.equals(currentscope)) ? GENERAL :
               (PROJECT.equals(currentscope) &&
                PRIVATE.equals(precisescope)) ? PROJECT : precisescope;
*/
        return currentscope;
    }

    public void loadSystemSharable(SharableElement sharable) {
        XModelEntity entity = sharable.getModelEntity();
        XChild[] children = entity.getChildren();
        for (int i = 0; i < children.length; i++) {
            if(children[i].isRequired()) {
                SharableElement c = (SharableElement)sharable.getModel().createModelObject(children[i].getName(), new Properties());
                if(c == null) {
                	ModelPlugin.getPluginLog().logInfo("Could not create sharable folder " + children[i].getName()); //$NON-NLS-1$
                	continue;
                }
                c.setScope(PROJECT);
                loadSystemSharable(c);
                sharable.addChild(c);
            }
        }
    }

}
