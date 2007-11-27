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
package org.jboss.tools.common.model.impl.trees;

import java.util.*;
import org.jboss.tools.common.model.*;

public abstract class DefaultFilteredTree implements XFilteredTree {

    protected XModel model = null;
    protected Set<String> entities = new HashSet<String>(10);
    protected Set<String> selectable_entities = new HashSet<String>(10);
    protected XModelObject root = null;

    public DefaultFilteredTree() {}
    
    public void dispose() {
    	if (entities!=null) entities.clear();
    	entities = null;
    	if (selectable_entities!=null) selectable_entities.clear();
    	selectable_entities = null;
    }

    public void setConstraint(Object object) {}

    protected void setRoot(XModelObject object) {
        root = object;
    }

    protected void addEntities(String[] _entities, boolean selectable) {
        addEntities(_entities);
        if(selectable) addSelectableEntities(_entities);
    }

    protected void addEntities(String[] _entities) {
        for (int i = 0; i < _entities.length; i++)
          entities.add(_entities[i]);
    }

    protected void addSelectableEntities(String[] _entities) {
        for (int i = 0; i < _entities.length; i++)
          selectable_entities.add(_entities[i]);
    }

    protected abstract void onSetModel();

    public void setModel(XModel model) {
        this.model = model;
        onSetModel();
    }

    public XModelObject getRoot() {
        return root;
    }

    public boolean hasChildren(XModelObject object) {
        return object.hasChildren();
    }

    public XModelObject[] getChildren(XModelObject object) {
        XModelObject[] cs = object.getChildren();
        ArrayList<XModelObject> v = new ArrayList<XModelObject>(cs.length);
        for(int i = 0; i < cs.length; i++) {
            if(isAcceptableChild(cs[i])) v.add(cs[i]);
        }
        return v.toArray(new XModelObject[0]);
    }

    public XModelObject getChildAt(XModelObject object, int i) {
        XModelObject[] c = getChildren(object);
        return (i < 0 || i >= c.length) ? null : c[i];
    }

    protected boolean isAcceptableChild(XModelObject child) {
        return (entities.contains(child.getModelEntity().getName()));
    }

    public boolean isSelectable(XModelObject object) {
        String e = object.getModelEntity().getName();
        return (selectable_entities.contains(e));
    }

    public String getValue(XModelObject object) {
        return belongs(object) ? object.getPath() : "";
    }

    public boolean belongs(XModelObject object) {
        String e = object.getModelEntity().getName();
        return entities.contains(e);
    }

    public XModelObject find(String value) {
        return model.getByPath(value);
    }

    public XModelObject getParent(XModelObject object) {
        return (object == root) ? null : object.getParent();
    }

    public String getPath(XModelObject object) {
        return object.getPath();
    }

}

