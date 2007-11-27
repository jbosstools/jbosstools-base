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

import java.util.*;
import org.w3c.dom.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.icons.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.icons.impl.*;

public class XModelMetaDataImpl implements XModelMetaData, XMetaDataConstants {
    private static XModelMetaDataImpl instance = null;

    private HashMap<String,XModelEntity> entities = new HashMap<String,XModelEntity>(50);
    private XIconListImpl icons = new XIconListImpl();
    private HashMap<String,XMappingImpl> mappings = new HashMap<String,XMappingImpl>();
    private XActionListImpl actions = new XActionListImpl();
    private XExtensions extensions = new XExtensions();
    private XParentsImpl parents = new XParentsImpl();
    
    static Object lock = new Object();

    public static XModelMetaData getInstance() {
    	if(instance == null) {
    		synchronized (lock) {
    			if(instance != null) return instance;
    			instance = new XModelMetaDataImpl();
    		}
    	}
        return instance;
    }

    private XModelMetaDataImpl() {
        try {
            XMetaDataLoader.loadMetaModel(this);
            instance = this;
            XModelEntityImpl r = (XModelEntityImpl)getEntity("Root");
            r.validateChildren();
            parents.init(this);
        } catch (Exception e) {
        	ModelPlugin.getPluginLog().logError(e);
            throw new RuntimeException("Cannot create metamodel: " + e.getMessage());
        }
    }

    public XIconList getIconList() {
        return icons;
    }

    public String[] entities() {
        return entities.keySet().toArray(new String[0]);
    }

    public XModelEntity getEntity(String entityname) {
        XModelEntityImpl entity = (XModelEntityImpl)entities.get(entityname);
        if(entity != null) entity.validate();
        return entity;
    }

    public XExtensions getExtensions() {
        return extensions;
    }

    public XModelEntity createEntity(Element element) {
        XModelEntityImpl entity = new XModelEntityImpl();
        entity.setElement(element);
        entities.put(entity.getName(), entity);
        return entity;
    }

    public XMapping getMapping(String name) {
        return mappings.get(name);
    }

    public void createIconList(Element element) {
        icons.load(element);
    }

    public void loadMappings(Element element) {
        Element[] es = XMetaDataLoader.getChildrenElements(element, MAPPING);
        for (int i = 0; i < es.length; i++) {
            String n = es[i].getAttribute(NAME);
            XMappingImpl m = mappings.get(n);
            if(m == null) {
                m = new XMappingImpl();
                mappings.put(n, m);
            }
            m.load(es[i]);
        }
    }

    public XActionList getGlobalActions() {
        return actions;
    }

    public void loadGlobalActions(Element element) {
        if(actions.getActionItems() == null) {
            actions.load(element);
        } else {
            XActionListImpl a = new XActionListImpl();
            a.load(element);
            actions.merge(a);
        }
    }

    private HashMap<String,String> loadedmodules = new HashMap<String,String>();

    public HashMap<String,String> getLoadedModules() {
        return loadedmodules;
    }
    
    /**
     * package local
     * @return
     */    
    Map<String,XModelEntity> getEntities() {
    	return entities;
    }

	public XParents getParentInfo() {
		return parents;
	}
    
}

