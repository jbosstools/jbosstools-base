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
package org.jboss.tools.common.meta.impl;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.*;
import org.w3c.dom.*;
import org.jboss.tools.common.meta.XMetaElement;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.xml.XMLUtilities;

public class XMetaDataLoader implements XMetaDataConstants {

    public static boolean hasAttribute(Element e, String s) {
        return XMLUtil.hasAttribute(e, s);
    }

    private static XMetaElement getDefaultMetaElementInstance(Class defImpl) {
        try {
            return (XMetaElementImpl)defImpl.newInstance();
        } catch (InstantiationException e) {
        	ModelPlugin.getPluginLog().logError("Error in getDefaultMetaElementInstance"); //$NON-NLS-1$
		} catch (IllegalAccessException e) {
			ModelPlugin.getPluginLog().logError("Error in getDefaultMetaElementInstance"); //$NON-NLS-1$
		}
		return null;
    }
    private static XMetaElement getMetaElementInstance(Element element, Class defImpl, boolean isRequired) {
        if(element == null) {
            return (!isRequired) ? null : getDefaultMetaElementInstance(defImpl);
        }
        if(defImpl != null) return getDefaultMetaElementInstance(defImpl);
        String loader = element.getAttribute(LOADER);
        if(loader == null || loader.length() == 0) return null;
        try {
            return (XMetaElementImpl)ClassLoaderUtil.getClassLoader().loadClass(loader).newInstance();
        } catch (InstantiationException e) {
        	ModelPlugin.getPluginLog().logError("Error in getMetaElementInstance " + loader, e);    //$NON-NLS-1$
		} catch (IllegalAccessException e) {
        	ModelPlugin.getPluginLog().logError("Error in getMetaElementInstance " + loader, e); //$NON-NLS-1$
		} catch (ClassNotFoundException e) {
        	ModelPlugin.getPluginLog().logError("Error in getMetaElementInstance " + loader, e); //$NON-NLS-1$
		}
		return null;
    }

    public static XMetaElement loadMetaElement(Element parent, String nodeName, Class defImpl, boolean isRequired){
        return loadMetaElement(getUniqueChild(parent, nodeName), defImpl, isRequired);
    }

    public static XMetaElement loadMetaElement(Element element, Class defImpl, boolean isRequired){
        XMetaElementImpl m = (XMetaElementImpl)getMetaElementInstance(element, defImpl, isRequired);
        if(m != null && element != null) {
            m.setName(element.getAttribute(NAME));
            m.setDisplayName(element.getAttribute(DISPLAYNAME));
            m.load(element);
        }
        return m;
    }

    public static XMetaElement[] loadElementGroup(Element element, String groupName, String elName, Class defImpl) {
        Element att = getUniqueChild(element, groupName);
        if(att == null) return (XMetaElement[])java.lang.reflect.Array.newInstance(defImpl,0);
        Element[] cs = getChildrenElements(att, elName);
        Vector<XMetaElement> v = new Vector<XMetaElement>(cs.length);
        for (int i = 0; i < cs.length; i++) {
             XMetaElement attr = loadMetaElement(cs[i], defImpl, false);
             if(attr != null) v.addElement(attr);
        }
        return v.toArray((XMetaElement[])java.lang.reflect.Array.newInstance(defImpl,v.size()));
    }

    public static Element getUniqueChild(Element element, String name) {
        return XMLUtil.getUniqueChild(element, name);
    }

    public static Element[] getChildrenElements(Element element, String child) {
        return XMLUtil.getChildren(element, child);
    }

    public static boolean getBoolean(Element el, String attName, boolean def){
        String a = el.getAttribute(attName);
        return (a == null || a.length() == 0) ? def :
               (XModelObjectConstants.TRUE.equalsIgnoreCase(a) || XModelObjectConstants.YES.equalsIgnoreCase(a));
    }

    public static int getInt(Element el, String attName, int def){
        try {
        	return Integer.parseInt(el.getAttribute(attName));
        } catch(NumberFormatException ex){
        	return def;
        }
    }

    // meta data loading

    static Element loadDocument(String filename) throws FileNotFoundException {
        Element e = XMLUtil.getElement(filename);
        if(e == null) throw new FileNotFoundException(MessageFormat.format("Can''t load meta model from {0}", filename));
        return e;
    }

    public static void loadMetaModel(XModelMetaDataImpl factory) {
        new MetaLibLoader().load(factory);
    }

    static void loadEntityGroup(XModelMetaDataImpl factory, Element g) {
//        String n = g.getAttribute(XModelObjectConstants.XML_ATTR_NAME);
    	String module = ""; //$NON-NLS-1$
    	Element v = XMLUtilities.getUniqueChild(g, "VERSION"); //$NON-NLS-1$
    	if(v != null && v.hasAttribute("MODULE")) { //$NON-NLS-1$
    		module = v.getAttribute("MODULE"); //$NON-NLS-1$
    	}
        loadMappings(factory, g);
        factory.createIconList(getUniqueChild(g, ICONS));
        loadGlobalActions(factory, g);
        loadEntities(factory, g, module);
    }

    private static void loadEntities(XModelMetaDataImpl factory, Element element, String module) {
        Element[] cs = getChildrenElements(element, XMODEL_ENTITY);
        for (int i = 0; i < cs.length; i++) {
        	factory.createEntity(cs[i], module);
        }
        cs = getChildrenElements(element, XENTITY_EXTENTION);
        XExtensions extensions = factory.getExtensions();
        for (int i = 0; i < cs.length; i++) extensions.addExtension(cs[i]);
    }

    private static void loadMappings(XModelMetaDataImpl factory, Element group) {
        Element mappings = getUniqueChild(group, MAPPINGS);
        if(mappings != null) factory.loadMappings(mappings);
    }

    private static void loadGlobalActions(XModelMetaDataImpl factory, Element group) {
        Element actions = getUniqueChild(group, "GlobalActions"); //$NON-NLS-1$
        if(actions != null) factory.loadGlobalActions(actions);
    }

}


