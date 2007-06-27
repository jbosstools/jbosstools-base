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
package org.jboss.tools.common.model.engines.impl;

import java.io.File;
import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.loaders.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class EnginesLoader extends URLRootLoader {

    public EnginesLoader() {}

    public void load(XModelObject object) {
        super.load(object);
        XChild[] cs = object.getModelEntity().getChildren();
        for (int i = 0; i < cs.length; i++) {
          if(!cs[i].isRequired() || cs[i].getMaxCount() != 1) continue;
          if(object.getChildren(cs[i].getName()).length > 0) continue;
          try {
              object.addChild(XModelObjectLoaderUtil.createValidObject(object.getModel(), cs[i].getName()));
          } catch (Exception e) {
        	  ModelPlugin.getPluginLog().logError("EnginesLoader:load:" + e.getMessage());
          }
        }
        if(isFilePath(getPath(object))) {
            File f = file(object);
            if(f.isFile()) {
                object.getModel().getFileRegistry().register(f);
            } else {
                object.getModel().getFileRegistry().unregister(f);
            }
        }
    }

    public boolean save(XModelObject object) {
        boolean b = super.save(object);
        if(b && isFilePath(getPath(object))) {
            File f = file(object);
            if(f.isFile()) object.getModel().getFileRegistry().register(f);
        }
        return b;
    }

    public boolean update(XModelObject object) {
        if(!isFilePath(getPath(object))) return true;
        File f = file(object);
        if(!object.getModel().getFileRegistry().isUpdated(f)) return true;
        XModelObject c = object.copy(0);
        load(c);
        merge(object, c);
        return true;
    }

    public String fileroot(XModelObject object) {
        return XModelConstants.getWorkspace(object.getModel()) + "/";
    }

    protected String fileName(XModelObject object) {
        return XModelConstants.getProjectPrefix(object.getModel()) +
               object.getModelEntity().getName().toLowerCase() + ".rex";
    }

	public static void merge(XModelObject object, XModelObject update) {
		merge(object, update, object.isActive());
	}

    public static void merge(XModelObject object, XModelObject update, boolean fire) {
		XModelObjectLoaderUtil.mergeAttributes(object, update, fire);
        Map<String,XModelObject> map = getChildrenForSaveAsMap(object);
        Set<String> set = null;
        XModelObject[] cs = update.getChildren();
        for (int i = 0; i < cs.length; i++) {
            XModelObject c = object.getChildByPath(cs[i].getPathPart());
            if(c == null) {
            	if(set == null) set = getChildrenToRemove(map, update);
            	c = findAppropriateChild(set, cs[i], map);
            	if(c == null) {
            		c = cs[i].copy();
            	} else {
            		merge(c, cs[i], fire);
            	}
                object.addChild(c);
            } else if(c.getModelEntity().getName().equals(cs[i].getModelEntity().getName())) {
                merge(c, cs[i], fire);
            } else {
            	object.removeChild(c);
            	object.addChild(cs[i].copy());
            }
			map.remove(c.getPathPart()); 
        }
        if(!map.isEmpty()) {
			Iterator it = map.values().iterator();
			while(it.hasNext()) {
				XModelObject o = (XModelObject)it.next();
				XChild childEntity = object.getModelEntity().getChild(o.getModelEntity().getName());
				if(childEntity != null && childEntity.isRequired()) {
					continue;
				} 
				o.removeFromParent();
			}
        }
        boolean doFire = false;
		for (int i = 0; i < cs.length; i++) {
			XModelObject c = object.getChildByPath(cs[i].getPathPart());
			if(c == null) continue;
			int ci = ((RegularObjectImpl)object).getIndexOfChild(c);
			if(ci == i) continue;
			doFire = true;
			((RegularObjectImpl)object).move(ci, i, false);
		}
		if(fire && doFire) ((XModelImpl)object.getModel()).fireStructureChanged(object);
    }
    
	static Map<String,XModelObject> emptyMap = new HashMap<String,XModelObject>();
	 
	private static Map<String,XModelObject> getChildrenForSaveAsMap(XModelObject object) {
		XModelObject[] cs = object.getChildrenForSave();
		if(cs.length == 0) return emptyMap;
		Map<String,XModelObject> map = new HashMap<String,XModelObject>();
		for (int i = 0; i < cs.length; i++) { 
			map.put(cs[i].getPathPart(), cs[i]);
		} 
		return map;
	}
	
	public static Set<String> getChildrenToRemove(Map save, XModelObject update) {
		Iterator it = save.keySet().iterator();
		Set<String> set = new HashSet<String>();
		while(it.hasNext()) {
			String p = it.next().toString();
			if(update.getChildByPath(p) == null) set.add(p); 
		}
		return set;
	}
	
	public static XModelObject findAppropriateChild(Set<String> set, XModelObject updateChild, Map<String,XModelObject> map) {
		String entity = updateChild.getModelEntity().getName();
		Iterator<String> it = set.iterator();
		while(it.hasNext()) {
			String p = it.next();
			XModelObject o = map.get(p);
			if(o == null) continue;
			if(entity.equals(o.getModelEntity().getName())) {
				it.remove();
				map.remove(p);
				return o;
			}
		}
		return null;
	}

}

