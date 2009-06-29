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

import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.icons.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.constraint.XAttributeConstraint;
import org.jboss.tools.common.meta.constraint.impl.XAttributeConstraintAList;
import org.jboss.tools.common.model.icons.impl.*;

public class XModelMetaDataImpl implements XModelMetaData, XMetaDataConstants {
    private static XModelMetaDataImpl instance = null;
    private static boolean loaded = false;
	boolean reportLoadingTime = false;

    private HashMap<String,XModelEntity> entities = new HashMap<String,XModelEntity>(50);
    private XIconListImpl icons = new XIconListImpl();
    private HashMap<String,XMappingImpl> mappings = new HashMap<String,XMappingImpl>();
    private XActionListImpl actions = new XActionListImpl();
    private XExtensions extensions = new XExtensions();
    private XParentsImpl parents = new XParentsImpl();
    
    static Object lock = new Object();

    public static XModelMetaData getInstance() {
    	if(loaded) return instance;
		synchronized (lock) {
   			if(instance != null) return instance;
   			instance = new XModelMetaDataImpl();
   			loaded = true;
    	}
        return instance;
    }

    private XModelMetaDataImpl() {
		long t = System.currentTimeMillis();
            XMetaDataLoader.loadMetaModel(this);
            instance = this;

            //uses cached elements
            parents.init(this);
            
            XModelEntityImpl r = (XModelEntityImpl)getEntity(XModelObjectConstants.ROOT_OBJECT);
            r.validateChildren();

            //Resolve XML now, or late resolving may not be thread safe. 
            XModelEntity[] es = this.entities.values().toArray(new XModelEntity[0]);
            for (int i = 0; i < es.length; i++) ((XModelEntityImpl)es[i]).validate();

//            makeStatistics(es);
		if(reportLoadingTime) {
    		long dt = - t + (t = System.currentTimeMillis());
			ModelPlugin.getPluginLog().logInfo("Meta model loaded in " + dt + " ms"); //$NON-NLS-1$ //$NON-NLS-2$
		}
    }

    private void makeStatistics(XModelEntity[] es) {
        Set<String> attNames = new HashSet<String>();
        Set<String> listValues = new HashSet<String>();
        Set<String> actionMenuNames = new HashSet<String>();
        int attrCount = 0;
        for (int i = 0; i < es.length; i++) {
        	XAttribute[] as = es[i].getAttributes();
        	for (int j = 0; j < as.length; j++) {
        		if(!as[j].isVisible()) continue;
            	attrCount++;
        		attNames.add(as[j].getName());
        		XAttributeConstraint c = as[j].getConstraint();
        		if(c instanceof XAttributeConstraintAList) {
        			String[] values = ((XAttributeConstraintAList)c).getValues();
        			for (int k = 0; k < values.length; k++)
        				listValues.add(values[k]);
        		}
        	}
        	
        	XActionList al = es[i].getActionList();
        	makeActionList(al, actionMenuNames);
        }
        System.out.println("------> Attributes=" + attNames.size() + " (total " + attrCount + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        System.out.println("------> List values=" + listValues.size()); //$NON-NLS-1$
        System.out.println("------> Action menu names=" + actionMenuNames.size()); //$NON-NLS-1$
    }

    private void makeActionList(XActionList list,  Set<String> actionMenuNames) {
    	XActionItem[] is = list.getActionItems();
    	for (int i = 0; i < is.length; i++) {
    		actionMenuNames.add(is[i].getName());
    		if(is[i] instanceof XActionList) {
    			makeActionList((XActionList)is[i], actionMenuNames);
    		}
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

    public XModelEntity createEntity(Element element, String module) {
        XModelEntityImpl entity = new XModelEntityImpl();
        entity.setModule(module);
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

